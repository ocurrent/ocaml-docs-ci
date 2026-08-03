(* Integration test: run odoc_driver_voodoo to generate docs for astring.

   Requires: base image cache, opam-repository
   Run with: DAY11_INTEGRATION=true dune exec day11/doc/test/test_generate_docs.exe *)

open Day11_test_util.Test_util

let cache_dir = Fpath.v "/tmp/day11-scratch-cache"
let os_dir = Fpath.(cache_dir / "linux-x86_64")
let _packages_dir = Fpath.(os_dir / "packages")
let base_dir = Fpath.(cache_dir / "base")
let _switch = "default"

let make_base () : Day11_layer.Base.t =
  {
    hash =
      Day11_opam_build.Base.build_hash ~os_distribution:"debian"
        ~os_version:"bookworm" ~arch:"x86_64" ();
    dir = base_dir;
    image = "debian:bookworm";
  }

let test_voodoo_astring () =
  with_eio @@ fun ~sw env ->
  if not (Bos.OS.Dir.exists base_dir |> Result.get_ok) then Alcotest.skip ();
  let opam_repository = opam_repository () in
  let base = make_base () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories [ (opam_repository, None) ]
  in
  (* Step 1: Build odoc-driver *)
  Printf.printf "Building odoc-driver.3.1.0...\n%!";
  let benv : Day11_opam_build.Types.build_env =
    { base; os_dir; uid = 1000; gid = 1000; cpu_slots = None }
  in
  let driver_tool =
    Day11_opam_build.Tools.build_tool ~sw env benv ~packages:git_packages
      ~repos:repos_with_shas
      (OpamPackage.of_string "odoc-driver.3.1.0")
    |> ok_or_fail "build odoc-driver"
  in
  Printf.printf "odoc-driver: %d layers\n%!" (List.length driver_tool.builds);
  (* Step 2: Build astring on top of driver layers *)
  Printf.printf "Building astring...\n%!";
  let find_opam pkg =
    try Some (Day11_opam.Git_packages.get_package git_packages pkg)
    with Not_found -> None
  in
  let astring_pkg = OpamPackage.of_string "astring.0.8.5" in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let astring_layer_hash =
    Day11_opam_build.Hash_cache.layer_hash cache ~base_hash:base.hash
      [ astring_pkg ]
  in
  let astring_node : Day11_opam_layer.Build.t =
    {
      hash = astring_layer_hash;
      pkg = astring_pkg;
      deps = driver_tool.builds;
      universe = Day11_solution.Universe.dummy;
    }
  in
  let astring_result =
    Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[]
      astring_node ()
  in
  (match astring_result with
  | Day11_opam_build.Types.Success bl -> Printf.printf "astring: %s\n%!" bl.hash
  | _ -> Alcotest.fail "astring build failed");
  (* Step 3: Create prep structure *)
  let astring_layer = Day11_opam_layer.Build.dir ~os_dir astring_node in
  let _ =
    Day11_sys.Sudo.run ~sw env
      Bos.Cmd.(
        v "chmod" % "-R" % "a+rX" % Fpath.to_string Fpath.(astring_layer / "fs"))
  in
  let installed_libs =
    Day11_opam_layer.Installed_files.scan_libs ~layer_dir:astring_layer
  in
  let installed_docs =
    Day11_opam_layer.Installed_files.scan_docs ~layer_dir:astring_layer
  in
  let prep_dir = Bos.OS.Dir.tmp "day11_prep_%s" |> Result.get_ok in
  let _prep_root =
    Day11_doc.Prep.create_with_mounts ~source_layer_dir:astring_layer
      ~dest_layer_dir:prep_dir ~universe:"test" ~pkg:astring_pkg ~installed_libs
      ~installed_docs
    |> ok_or_fail "prep"
  in
  (* Step 4: Run odoc_driver_voodoo via Run_in_layers *)
  let all_builds =
    driver_tool.builds
    @
    match astring_result with
    | Day11_opam_build.Types.Success bl -> [ bl ]
    | _ -> []
  in
  let voodoo_cmd =
    "eval $(opam env) && "
    ^ "odoc_driver_voodoo astring "
    ^ "--odoc-dir /home/opam/odoc-out "
    ^ "--html-dir /home/opam/html "
    ^ "--actions all -v"
  in
  let mounts =
    [
      Day11_container.Mount.bind_ro
        ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
        "/home/opam/prep";
    ]
  in
  let build_dirs = List.map (Day11_opam_layer.Build.dir ~os_dir) all_builds in
  let spec =
    Day11_opam_build.Build_layer.opam_build_spec ~cmd:voodoo_cmd ~mounts
      ~uid:1000 ~gid:1000 ()
  in
  let run, upper, _timing =
    Day11_runner.Run_in_layers.run ~sw env ~base ~build_dirs spec
    |> ok_or_fail "run voodoo"
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Day11_sys.Sudo.rm_rf ~sw env (Fpath.parent upper));
      ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir))
    (fun () ->
      let exit_code =
        match run.status with `Exited n -> n | `Signaled n -> 128 + n
      in
      Printf.printf "odoc_driver_voodoo exit: %d (%.1fs)\n%!" exit_code run.time;
      if exit_code <> 0 then Printf.printf "STDERR:\n%s\n%!" run.errors;
      let _ =
        Day11_sys.Sudo.run ~sw env
          Bos.Cmd.(v "chmod" % "-R" % "a+rX" % Fpath.to_string upper)
      in
      let html_dir = Fpath.(upper / "home" / "opam" / "html") in
      let html_count =
        if Bos.OS.Dir.exists html_dir |> Result.get_ok then (
          let find_run =
            Day11_sys.Run.run ~sw env
              Bos.Cmd.(
                v "find"
                % Fpath.to_string html_dir
                % "-name"
                % "*.html"
                % "-type"
                % "f")
              None
          in
          let files =
            String.split_on_char '\n' (String.trim find_run.output)
            |> List.filter (fun s -> s <> "")
          in
          Printf.printf "HTML files: %d\n%!" (List.length files);
          List.length files)
        else 0
      in
      Alcotest.(check bool) "voodoo exit 0" true (exit_code = 0);
      Alcotest.(check bool) "HTML generated" true (html_count > 0))

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_generate_docs"
      [
        ( "Voodoo",
          [ Alcotest.test_case "astring docs" `Slow test_voodoo_astring ] );
      ]
