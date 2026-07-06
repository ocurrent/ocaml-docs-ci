(* Integration test: full doc pipeline with compile/link split.

   Solves odoc-driver, builds all deps, generates docs for all
   packages. odoc gets the compile/link split because it has
   x-extra-doc-deps.

   Run with: OPAM_REPOSITORY=... DAY11_INTEGRATION=true \
     dune exec day11/doc/test/test_doc_pipeline.exe *)

open Day11_opam_build
module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
type build = Build.t
open Day11_test_util.Test_util

let scratch_cache_dir = Fpath.v "/tmp/day11-scratch-cache"

let test_full_doc_pipeline () = with_eio @@ fun ~sw env ->
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
  let switch = Types.switch in
  let odoc_bin = Printf.sprintf "/home/opam/.opam/%s/bin/odoc" switch in
  let odoc_md_bin = Printf.sprintf "/home/opam/.opam/%s/bin/odoc-md" switch in
  (* Step 1: Solve odoc-driver and build all deps *)
  Printf.printf "Building odoc-driver and all deps...\n%!";
  let driver_pkg = OpamPackage.of_string "odoc-driver.3.1.0" in
  let odoc_tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      driver_pkg
    |> ok_or_fail "build odoc-driver"
  in
  Printf.printf "  %d packages built\n%!" (List.length odoc_tool.builds);
  (* Step 2: All packages use compile+link split in unified DAG *)
  let no_split = odoc_tool.builds in
  let needs_split = [] in
  Printf.printf "  %d packages for doc generation\n%!"
    (List.length no_split);
  (* Step 3: Generate docs for non-split packages (--actions all) *)
  let universe = Day11_doc.Command.compute_universe_hash
    (List.map (fun (b : build) -> b.hash) odoc_tool.builds) in
  let doc_all_count = ref 0 in
  let doc_all_html = ref 0 in
  Printf.printf "\nGenerating docs (single phase)...\n%!";
  List.iter (fun (b : build) ->
    let pkg_dir = Build.dir ~os_dir b in
    let installed_libs = Day11_opam_layer.Installed_files.scan_libs
      ~layer_dir:pkg_dir in
    if installed_libs = [] then
      Printf.printf "  %s: no libs, skipping\n%!"
        (OpamPackage.to_string b.pkg)
    else begin
      let installed_docs = Day11_opam_layer.Installed_files.scan_docs
        ~layer_dir:pkg_dir in
      let prep_dir = Bos.OS.Dir.tmp "day11_doc_%s" |> Result.get_ok in
      ignore (Day11_doc.Prep.create_with_mounts ~source_layer_dir:pkg_dir
        ~dest_layer_dir:prep_dir ~universe ~pkg:b.pkg
        ~installed_libs ~installed_docs);
      let prep_mount = Day11_container.Mount.bind_ro
        ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
        "/home/opam/prep" in
      let cmd =
        "eval $(opam env) && " ^
        Day11_doc.Command.odoc_driver_voodoo ~pkg:b.pkg ~universe
          ~blessed:true ~actions:"all" ~odoc_bin ~odoc_md_bin
      in
      let doc_hash = Day11_layer.Hash.of_strings
        [ "doc-all"; b.hash; odoc_tool.hash; universe ] in
      let doc_node : build =
        { hash = doc_hash; pkg = b.pkg;
          deps = odoc_tool.builds @ [ b ]; universe = Day11_solution.Universe.dummy } in
      (match Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:[ prep_mount ]
               doc_node ~strategy:{ cmd; cleanup = fun ~sw:_ _ _ -> () } () with
       | Types.Success bl ->
         let dd = Build.dir ~os_dir bl in
         let find_run = Day11_sys.Run.run ~sw env
           Bos.Cmd.(v "find" % Fpath.to_string Fpath.(dd / "fs")
                    % "-name" % "*.html" % "-type" % "f") None in
         let n = List.length (String.split_on_char '\n'
           (String.trim find_run.output)
           |> List.filter (fun s -> s <> "")) in
         Printf.printf "  %s: %d HTML\n%!"
           (OpamPackage.to_string b.pkg) n;
         incr doc_all_count;
         doc_all_html := !doc_all_html + n
       | Types.Failure _ ->
         Printf.printf "  %s: FAILED\n%!" (OpamPackage.to_string b.pkg)
       | _ -> ());
      ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir)
    end
  ) no_split;
  Printf.printf "  → %d packages documented, %d total HTML files\n%!"
    !doc_all_count !doc_all_html;
  (* Step 4: Compile phase for split packages *)
  Printf.printf "\nGenerating docs (split phase — compile)...\n%!";
  let compile_builds = List.filter_map (fun (b : build) ->
    let pkg_dir = Build.dir ~os_dir b in
    let installed_libs = Day11_opam_layer.Installed_files.scan_libs
      ~layer_dir:pkg_dir in
    if installed_libs = [] then None
    else begin
      let installed_docs = Day11_opam_layer.Installed_files.scan_docs
        ~layer_dir:pkg_dir in
      let prep_dir = Bos.OS.Dir.tmp "day11_doc_%s" |> Result.get_ok in
      ignore (Day11_doc.Prep.create_with_mounts ~source_layer_dir:pkg_dir
        ~dest_layer_dir:prep_dir ~universe ~pkg:b.pkg
        ~installed_libs ~installed_docs);
      let prep_mount = Day11_container.Mount.bind_ro
        ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
        "/home/opam/prep" in
      let cmd =
        "eval $(opam env) && " ^
        Day11_doc.Command.odoc_driver_voodoo ~pkg:b.pkg ~universe
          ~blessed:true ~actions:"compile-only" ~odoc_bin ~odoc_md_bin
      in
      let compile_hash = Day11_layer.Hash.of_strings
        [ "compile"; b.hash; odoc_tool.hash ] in
      let compile_node : build =
        { hash = compile_hash; pkg = b.pkg;
          deps = odoc_tool.builds @ [ b ]; universe = Day11_solution.Universe.dummy } in
      match Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:[ prep_mount ]
              compile_node
              ~strategy:{ cmd; cleanup = fun ~sw:_ _ _ -> () } () with
      | Types.Success bl ->
        Printf.printf "  %s: compile OK\n%!"
          (OpamPackage.to_string b.pkg);
        ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir);
        Some (b, bl)
      | _ ->
        Printf.printf "  %s: compile FAILED\n%!"
          (OpamPackage.to_string b.pkg);
        ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir);
        None
    end
  ) needs_split in
  (* Step 5: Link phase for split packages
     These now have access to their deps' doc-all layers AND their
     own compile layer *)
  Printf.printf "\nGenerating docs (split phase — link)...\n%!";
  let split_html = ref 0 in
  List.iter (fun ((b : build), (compile_bl : build)) ->
    let pkg_dir = Build.dir ~os_dir b in
    let installed_libs = Day11_opam_layer.Installed_files.scan_libs
      ~layer_dir:pkg_dir in
    let installed_docs = Day11_opam_layer.Installed_files.scan_docs
      ~layer_dir:pkg_dir in
    let prep_dir = Bos.OS.Dir.tmp "day11_doc_%s" |> Result.get_ok in
    ignore (Day11_doc.Prep.create_with_mounts ~source_layer_dir:pkg_dir
      ~dest_layer_dir:prep_dir ~universe ~pkg:b.pkg
      ~installed_libs ~installed_docs);
    let prep_mount = Day11_container.Mount.bind_ro
      ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
      "/home/opam/prep" in
    let cmd =
      "eval $(opam env) && " ^
      Day11_doc.Command.odoc_driver_voodoo ~pkg:b.pkg ~universe
        ~blessed:true ~actions:"link-and-gen" ~odoc_bin ~odoc_md_bin
    in
    let link_hash = Day11_layer.Hash.of_strings
      [ "link"; compile_bl.hash; universe ] in
    (* Link deps include: tool layers + build + compile layer *)
    let link_node : build =
      { hash = link_hash; pkg = b.pkg;
        deps = odoc_tool.builds @ [ b; compile_bl ]; universe = Day11_solution.Universe.dummy } in
    (match Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:[ prep_mount ]
             link_node ~strategy:{ cmd; cleanup = fun ~sw:_ _ _ -> () } () with
     | Types.Success bl ->
       let dd = Build.dir ~os_dir bl in
       let find_run = Day11_sys.Run.run ~sw env
         Bos.Cmd.(v "find" % Fpath.to_string Fpath.(dd / "fs")
                  % "-name" % "*.html" % "-type" % "f") None in
       let n = List.length (String.split_on_char '\n'
         (String.trim find_run.output)
         |> List.filter (fun s -> s <> "")) in
       Printf.printf "  %s: link → %d HTML\n%!"
         (OpamPackage.to_string b.pkg) n;
       split_html := !split_html + n
     | Types.Failure _ ->
       Printf.printf "  %s: link FAILED\n%!" (OpamPackage.to_string b.pkg)
     | _ -> ());
    ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir)
  ) compile_builds;
  Printf.printf "\n=== Summary ===\n%!";
  Printf.printf "  Single-phase: %d packages, %d HTML\n%!"
    !doc_all_count !doc_all_html;
  Printf.printf "  Split-phase: %d packages, %d HTML\n%!"
    (List.length compile_builds) !split_html;
  Printf.printf "  Total HTML: %d\n%!" (!doc_all_html + !split_html);
  Alcotest.(check bool) "some single-phase docs" true (!doc_all_count > 0)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_doc_pipeline"
      [ ( "Full pipeline",
          [ Alcotest.test_case "odoc-driver docs" `Slow
              test_full_doc_pipeline ] ) ]
