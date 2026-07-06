(* Integration test: doc compile/link phases.

   Uses bind-mounted tool binaries instead of stacking tool layers,
   so only the package's real deps are in the overlay.

   Requires: from-scratch cache at /tmp/day11-scratch-cache
   Run with: OPAM_REPOSITORY=... DAY11_INTEGRATION=true \
     dune exec day11/doc/test/test_doc_compile_link.exe *)

open Day11_opam_build
module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
type build = Build.t
open Day11_test_util.Test_util

let scratch_cache_dir = Fpath.v "/tmp/day11-scratch-cache"

(** Build docs for a package using bind-mounted tool binaries *)
let build_docs ~sw env benv ~os_dir ~odoc_tool ~pkg_build ~pkg =
  let tool_mounts, odoc_bin, odoc_md_bin =
    Day11_doc.Tool_binaries.doc_tool_mounts odoc_tool in
  (* odoc_driver_voodoo is also bind-mounted *)
  let voodoo_bin = "/home/opam/doc-tools/bin/odoc_driver_voodoo" in
  let pkg_dir = Build.dir ~os_dir pkg_build in
  let installed_libs = Day11_opam_layer.Installed_files.scan_libs
    ~layer_dir:pkg_dir in
  let installed_docs = Day11_opam_layer.Installed_files.scan_docs
    ~layer_dir:pkg_dir in
  let universe = Day11_doc.Command.compute_universe_hash
    (List.map (fun (b : build) -> b.hash) odoc_tool.builds) in
  Printf.printf "  prep: %d lib files, %d doc files\n%!"
    (List.length installed_libs) (List.length installed_docs);
  let prep_dir = Bos.OS.Dir.tmp "day11_doctest_%s" |> Result.get_ok in
  let _prep_root =
    Day11_doc.Prep.create_with_mounts
      ~source_layer_dir:pkg_dir ~dest_layer_dir:prep_dir
      ~universe ~pkg
      ~installed_libs ~installed_docs
    |> ok_or_fail "prep"
  in
  let prep_mount = Day11_container.Mount.bind_ro
    ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
    "/home/opam/prep" in
  let all_mounts = prep_mount :: tool_mounts in
  (* Use the bind-mounted voodoo binary directly *)
  let make_cmd actions =
    Printf.sprintf
      "eval $(opam env) && %s %s \
       --odoc-dir /home/opam/odoc-out \
       --html-dir /home/opam/html \
       --actions %s -j $(nproc) -v \
       --blessed \
       --odoc %s \
       --odoc-md %s"
      voodoo_bin
      (OpamPackage.name_to_string pkg)
      actions odoc_bin odoc_md_bin
  in
  (* Compile — only package's real deps, no tool layers *)
  let compile_cmd = make_cmd "compile-only" in
  let compile_hash = Day11_layer.Hash.of_strings
    [ "compile"; pkg_build.hash; odoc_tool.hash ] in
  let compile_node : build =
    { hash = compile_hash; pkg;
      deps = pkg_build.deps @ [ pkg_build ]; universe = Day11_solution.Universe.dummy } in
  let compile_build = match
    Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:all_mounts
      compile_node ~strategy:{ cmd = compile_cmd; cleanup = fun ~sw:_ _ _ -> () } ()
  with
    | Types.Success bl ->
      let cd = Build.dir ~os_dir bl in
      let find_run = Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "find" % Fpath.to_string Fpath.(cd / "fs")
                 % "-name" % "*.odoc" % "-type" % "f") None in
      let n = List.length (String.split_on_char '\n' (String.trim find_run.output)
        |> List.filter (fun s -> s <> "")) in
      Printf.printf "  compile: %d .odoc files\n%!" n;
      bl
    | Types.Failure name ->
      let log = Fpath.(Build.dir ~os_dir compile_node / "layer.log") in
      (match Bos.OS.File.read log with
       | Ok s -> Printf.printf "COMPILE LOG:\n%s\n%!" s | Error _ -> ());
      Alcotest.fail ("compile failed: " ^ name)
    | _ -> Alcotest.fail "compile unexpected"
  in
  (* Link — same package deps + compile layer, still no tool layers *)
  let link_cmd = make_cmd "link-and-gen" in
  let link_hash = Day11_layer.Hash.of_strings
    [ "link"; compile_build.hash; universe ] in
  let link_node : build =
    { hash = link_hash; pkg;
      deps = pkg_build.deps @ [ pkg_build; compile_build ]; universe = Day11_solution.Universe.dummy } in
  let html_count = match
    Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:all_mounts
      link_node ~strategy:{ cmd = link_cmd; cleanup = fun ~sw:_ _ _ -> () } ()
  with
    | Types.Success bl ->
      let ld = Build.dir ~os_dir bl in
      let find_run = Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "find" % Fpath.to_string Fpath.(ld / "fs")
                 % "-name" % "*.html" % "-type" % "f") None in
      let files = String.split_on_char '\n' (String.trim find_run.output)
        |> List.filter (fun s -> s <> "") in
      Printf.printf "  link: %d HTML files\n%!" (List.length files);
      List.length files
    | Types.Failure name ->
      let log = Fpath.(Build.dir ~os_dir link_node / "layer.log") in
      (match Bos.OS.File.read log with
       | Ok s -> Printf.printf "LINK LOG:\n%s\n%!" s | Error _ -> ());
      Alcotest.fail ("link failed: " ^ name)
    | _ -> Alcotest.fail "link unexpected"
  in
  ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir);
  html_count

let setup () =
  let base = match Base.load_cached
    ~os_dir:Fpath.(scratch_cache_dir / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
    | Some b -> b
    | None -> Printf.printf "No cache\n%!"; Alcotest.skip ()
  in
  let opam_repository = opam_repository () in
  let os_dir = Fpath.(scratch_cache_dir / "linux-x86_64") in
  let benv = Types.make_build_env ~base ~os_dir ~uid:1000 ~gid:1000 () in
  Types.ensure_dirs benv;
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  (base, os_dir, benv, git_packages, repos_with_shas, opam_env)

let test_astring_docs () = with_eio @@ fun ~sw env ->
  let base, os_dir, benv, git_packages, repos_with_shas, opam_env = setup () in
  Printf.printf "Building odoc-driver tools...\n%!";
  let odoc_tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      (OpamPackage.of_string "odoc-driver.3.1.0")
    |> ok_or_fail "build odoc-driver"
  in
  Printf.printf "  odoc-driver: %d layers\n%!" (List.length odoc_tool.builds);
  Printf.printf "Building astring...\n%!";
  let astring_pkg = OpamPackage.of_string "astring.0.8.5" in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Hash_cache.create ~find_opam () in
  let astring_solution = match Day11_solver.Solve.solve ~packages:git_packages
    ~env:opam_env astring_pkg with
    | Ok result -> result.Day11_solution.Solve_result.build_deps
    | Error (e, _) -> Alcotest.fail ("solve astring: " ^ e) in
  let astring_nodes = Dag.build_dag cache ~base_hash:base.hash
    [ (astring_pkg, astring_solution, astring_solution) ] in
  Dag_executor.execute env ~np:4
    ~on_complete:(fun ~stats:_ ~cached:_ _ _ -> ())
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
    astring_nodes
    (fun node -> match Build_layer.build ~sw env benv ~opam_repositories:[] node () with
      | Types.Success _ -> true | _ -> false);
  let astring_build = List.find (fun (n : build) ->
    OpamPackage.equal n.pkg astring_pkg) astring_nodes in
  Printf.printf "Generating astring docs (bind-mounted tools)...\n%!";
  let html = build_docs ~sw env benv ~os_dir ~odoc_tool
    ~pkg_build:astring_build ~pkg:astring_pkg in
  Alcotest.(check bool) "astring has HTML" true (html > 0)

let test_odoc_docs () = with_eio @@ fun ~sw env ->
  let _base, os_dir, benv, git_packages, repos_with_shas, _opam_env = setup () in
  Printf.printf "Building odoc-driver tools...\n%!";
  let odoc_tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      (OpamPackage.of_string "odoc-driver.3.1.0")
    |> ok_or_fail "build odoc-driver"
  in
  Printf.printf "  odoc-driver: %d layers\n%!" (List.length odoc_tool.builds);
  let odoc_pkg = OpamPackage.of_string "odoc.3.1.0" in
  let odoc_build = match List.find_opt (fun (b : build) ->
    OpamPackage.equal b.pkg odoc_pkg) odoc_tool.builds with
    | Some b -> b
    | None -> Alcotest.fail "odoc not found in tool builds"
  in
  Printf.printf "Generating odoc docs (bind-mounted tools, %d real deps)...\n%!"
    (List.length odoc_build.deps);
  let html = build_docs ~sw env benv ~os_dir ~odoc_tool
    ~pkg_build:odoc_build ~pkg:odoc_pkg in
  Printf.printf "  odoc: %d HTML files\n%!" html;
  Alcotest.(check bool) "odoc has HTML" true (html > 0)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_doc_compile_link"
      [ ( "Doc phases",
          [ Alcotest.test_case "astring docs" `Slow test_astring_docs;
            Alcotest.test_case "odoc docs" `Slow test_odoc_docs ] ) ]
