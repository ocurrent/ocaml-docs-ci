(* Integration test: build JTW toolchain.

   Step 1: Build js_of_ocaml via Tools.build_tool (compiler + jsoo deps)
   Step 2: Build full JTW tools layer (pin js_top_worker, build worker.js)

   Requires: from-scratch cache at /tmp/day11-scratch-cache, network
   Run with: DAY11_INTEGRATION=true dune exec day11/jtw/test/test_jtw_integration.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let scratch_cache_dir = Fpath.v "/tmp/day11-scratch-cache"
let jtw_local_source = "/home/jjl25/monopam-myspace/js_top_worker"
let jtw_container_path = "/home/opam/local/js_top_worker"

let test_build_jsoo () = with_eio @@ fun ~sw env ->
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
  let jsoo_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "js_of_ocaml") in
  let jsoo_pkg =
    match OpamPackage.Version.Map.max_binding_opt jsoo_versions with
    | Some (v, _) ->
      OpamPackage.create (OpamPackage.Name.of_string "js_of_ocaml") v
    | None -> Alcotest.skip ()
  in
  Printf.printf "Building %s...\n%!" (OpamPackage.to_string jsoo_pkg);
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      jsoo_pkg
    |> ok_or_fail "build_tool"
  in
  Printf.printf "  %d layers\n%!" (List.length tool.builds);
  Alcotest.(check bool) "has layers" true
    (List.length tool.builds > 0);
  let has_jsoo_bin = List.exists (fun (bl : Day11_opam_layer.Build.t) ->
    let bl_dir = Day11_opam_layer.Build.dir ~os_dir bl in
    let bin = Fpath.(bl_dir / "fs" / "home" / "opam" / ".opam"
                     / Types.switch / "bin" / "js_of_ocaml") in
    Bos.OS.File.exists bin |> Result.get_ok
  ) tool.builds in
  Printf.printf "  js_of_ocaml binary found: %b\n%!" has_jsoo_bin;
  Alcotest.(check bool) "js_of_ocaml binary" true has_jsoo_bin

let test_build_jtw_tools () = with_eio @@ fun ~sw env ->
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
  (* First build the compiler + jsoo deps via Tools.build_tool *)
  Printf.printf "Building js_of_ocaml toolchain...\n%!";
  let jsoo_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "js_of_ocaml") in
  let jsoo_pkg =
    match OpamPackage.Version.Map.max_binding_opt jsoo_versions with
    | Some (v, _) ->
      OpamPackage.create (OpamPackage.Name.of_string "js_of_ocaml") v
    | None -> Alcotest.skip ()
  in
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      jsoo_pkg
    |> ok_or_fail "jsoo build"
  in
  Printf.printf "  jsoo: %d layers\n%!" (List.length tool.builds);
  (* Now build the full JTW tools layer on top, using local source *)
  if not (Sys.file_exists jtw_local_source) then
    (Printf.printf "No local js_top_worker at %s\n%!" jtw_local_source;
     Alcotest.skip ());
  Printf.printf "Building JTW tools (local: %s)...\n%!" jtw_local_source;
  let extra_pins = Day11_jtw.Tool_layer.[
    { package = "mime_printer";
      url = "git+https://github.com/jonludlam/mime_printer.git#odoc_notebook" }
  ] in
  let cmd = Day11_jtw.Tool_layer.build_cmd_local
    ~container_path:jtw_container_path ~extra_pins in
  let strategy : Types.build_strategy = {
    cmd;
    cleanup = Build_layer.opam_build_cleanup;
  } in
  let jtw_mount = Day11_container.Mount.bind_ro
    ~src:jtw_local_source jtw_container_path in
  (* Use a dummy package for the layer *)
  let jtw_pkg = OpamPackage.of_string "jtw-tools.0" in
  let layer_hash = Day11_layer.Hash.of_strings
    [ "jtw-tools"; base.hash; jtw_local_source ] in
  let jtw_node : Day11_opam_layer.Build.t =
    { hash = layer_hash; pkg = jtw_pkg; deps = tool.builds; universe = Day11_solution.Universe.dummy } in
  let result =
    Build_layer.build ~sw env benv
      ~opam_repositories:[]
      ~mounts:[ jtw_mount ]
      jtw_node
      ~strategy ()
  in
  match result with
  | Types.Success bl ->
    Printf.printf "  JTW tools layer: %s\n%!" bl.hash;
    let bl_dir = Day11_opam_layer.Build.dir ~os_dir bl in
    (* Check for jtw binary *)
    let has_jtw = Bos.OS.File.exists
      Fpath.(bl_dir / "fs" / "home" / "opam" / ".opam"
             / Types.switch / "bin" / "jtw")
      |> Result.get_ok in
    Printf.printf "  jtw binary: %b\n%!" has_jtw;
    Alcotest.(check bool) "jtw binary" true has_jtw;
    (* Check for stdlib artifacts (--no-worker produces CMIs, not worker.js) *)
    let dynamic_cmis = Fpath.(bl_dir / "fs" / "home" / "opam"
      / "jtw-tools-output" / "lib" / "ocaml" / "dynamic_cmis.json") in
    let has_dynamic_cmis = Bos.OS.File.exists dynamic_cmis |> Result.get_ok in
    Printf.printf "  dynamic_cmis.json: %b\n%!" has_dynamic_cmis;
    Alcotest.(check bool) "dynamic_cmis.json" true has_dynamic_cmis;
    let stdlib_js = Fpath.(bl_dir / "fs" / "home" / "opam"
      / "jtw-tools-output" / "lib" / "ocaml" / "stdlib.cma.js") in
    let has_stdlib_js = Bos.OS.File.exists stdlib_js |> Result.get_ok in
    Printf.printf "  stdlib.cma.js: %b\n%!" has_stdlib_js;
    Alcotest.(check bool) "stdlib.cma.js" true has_stdlib_js
  | Types.Failure name ->
    (* Print the build log for debugging *)
    let log_path = Fpath.(benv.os_dir / name / "layer.log") in
    (match Bos.OS.File.read log_path with
     | Ok log -> Printf.printf "BUILD LOG:\n%s\n%!" log
     | Error _ -> ());
    Alcotest.fail (Printf.sprintf "JTW tools build failed: %s" name)
  | _ ->
    Alcotest.fail "Unexpected build result"

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_jtw_integration"
      [ ( "JTW",
          [ Alcotest.test_case "build js_of_ocaml" `Slow
              test_build_jsoo;
            Alcotest.test_case "build jtw tools" `Slow
              test_build_jtw_tools ] ) ]
