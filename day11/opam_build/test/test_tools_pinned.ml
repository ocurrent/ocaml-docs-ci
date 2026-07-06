(* Integration test: build tools from pinned source dirs and override repos.

   Tests:
   1. Build odoc-parser from a local source dir (simulates --odoc-repo)
   2. Build a tool using multiple repos with overlay semantics

   Requires:
   - base image cache at CACHE_DIR (default /tmp/day11-scratch-cache)
   - opam-repository at OPAM_REPOSITORY
   - odoc source at ODOC_REPO (default ~/odoc)

   Run with:
     DAY11_INTEGRATION=true \
     OPAM_REPOSITORY=~/opam-repository \
     ODOC_REPO=~/odoc \
     dune exec day11/opam_build/test/test_tools_pinned.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let cache_dir =
  Fpath.v (match Sys.getenv_opt "CACHE_DIR" with
    | Some d -> d
    | None ->
      let home = Sys.getenv "HOME" in
      Filename.concat home "cache-day11-ox2")

let os_dir = Fpath.(cache_dir / "linux-x86_64")

let odoc_repo () =
  match Sys.getenv_opt "ODOC_REPO" with
  | Some path when Sys.file_exists path -> path
  | _ ->
    let default = Filename.concat (Sys.getenv "HOME") "odoc" in
    if Sys.file_exists default then default
    else (Printf.printf "ODOC_REPO not set and ~/odoc doesn't exist\n%!";
          Alcotest.skip ())

let make_build_env () =
  match Base.load_cached ~os_dir
    ~os_distribution:"debian" ~os_version:"bookworm" with
  | Some base -> Types.make_build_env ~base ~os_dir ()
  | None ->
    Printf.printf "No cached base image at %s\n%!" (Fpath.to_string cache_dir);
    Alcotest.skip ()

let read_pins dir =
  let opam_files = Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".opam") in
  List.fold_left (fun acc filename ->
    let name = Filename.chop_suffix filename ".opam" in
    let path = Filename.concat dir filename in
    try
      let opam = OpamFile.OPAM.read
        (OpamFile.make (OpamFilename.raw path)) in
      OpamPackage.Name.Map.add
        (OpamPackage.Name.of_string name)
        (OpamPackage.Version.of_string "dev", opam) acc
    with _ -> acc
  ) OpamPackage.Name.Map.empty opam_files

(* Test 1: Build odoc-parser from local source *)
let test_build_odoc_parser_from_source () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let odoc_dir = odoc_repo () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let pins = read_pins odoc_dir in
  let source_dirs = OpamPackage.Name.Map.fold (fun name _ acc ->
    OpamPackage.Name.Map.add name odoc_dir acc
  ) pins OpamPackage.Name.Map.empty in
  Printf.printf "Pins from %s: %s\n%!" odoc_dir
    (String.concat ", " (List.map OpamPackage.Name.to_string
      (OpamPackage.Name.Map.fold (fun n _ acc -> n :: acc) pins [])));
  let target = OpamPackage.of_string "odoc-parser.dev" in
  Printf.printf "Building %s from source...\n%!"
    (OpamPackage.to_string target);
  Printf.printf "Attempting build...\n%!";
  let result = Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      ~pin_dirs:[ odoc_dir ] ~source_dirs target in
  (match result with
   | Error (`Msg e) -> Printf.printf "Build error: %s\n%!" e
   | Ok _ -> Printf.printf "Build succeeded!\n%!");
  let tool = result |> ok_or_fail "build_tool odoc-parser.dev"
  in
  Printf.printf "odoc-parser.dev built: %d layers\n%!"
    (List.length tool.builds);
  (* Check that odoc-parser was actually installed *)
  let lib_dir = Fpath.(tool.dir / "fs" / "home" / "opam" / ".opam"
    / Types.switch / "lib" / "odoc-parser") in
  let has_lib = Bos.OS.Dir.exists lib_dir |> Result.get_ok in
  Printf.printf "odoc-parser lib dir exists: %b (%s)\n%!"
    has_lib (Fpath.to_string lib_dir);
  Alcotest.(check bool) "odoc-parser installed" true has_lib

(* Test 2: Build odoc-driver from local source (full tool chain) *)
let test_build_odoc_driver_from_source () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let odoc_dir = odoc_repo () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let pins = read_pins odoc_dir in
  let source_dirs = OpamPackage.Name.Map.fold (fun name _ acc ->
    OpamPackage.Name.Map.add name odoc_dir acc
  ) pins OpamPackage.Name.Map.empty in
  let target = OpamPackage.of_string "odoc-driver.dev" in
  Printf.printf "Building %s from source...\n%!"
    (OpamPackage.to_string target);
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      ~pin_dirs:[ odoc_dir ] ~source_dirs target
    |> ok_or_fail "build_tool odoc-driver.dev"
  in
  Printf.printf "odoc-driver.dev built: %d layers\n%!"
    (List.length tool.builds);
  (* Check that all doc binaries exist across built layers *)
  let os_dir = Fpath.(cache_dir / "linux-x86_64") in
  let find_binary name =
    List.exists (fun (bl : Day11_opam_layer.Build.t) ->
      let dir = Day11_opam_layer.Build.dir ~os_dir bl in
      let bin = Fpath.(dir / "fs" / "home" / "opam" / ".opam"
        / Types.switch / "bin" / name) in
      Bos.OS.File.exists bin |> Result.get_ok
    ) tool.builds
  in
  let required_binaries = [ "odoc"; "odoc-md"; "sherlodoc"; "odoc_driver" ] in
  List.iter (fun name ->
    let found = find_binary name in
    Printf.printf "  %s: %b\n%!" name found;
    Alcotest.(check bool) (name ^ " binary") true found
  ) required_binaries;
  (* Also check that pinned packages were built from source *)
  let pinned_pkgs = [ "odoc"; "odoc-parser"; "odoc-md"; "sherlodoc";
                       "odoc-driver" ] in
  List.iter (fun name ->
    let found = List.exists (fun (bl : Day11_opam_layer.Build.t) ->
      let pkg_name = OpamPackage.Name.to_string (OpamPackage.name bl.pkg) in
      pkg_name = name
    ) tool.builds in
    Printf.printf "  %s built: %b\n%!" name found;
    Alcotest.(check bool) (name ^ " built") true found
  ) pinned_pkgs

(* Test 3: Build using multiple repos with overlay *)
let test_build_with_multi_repo () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let benv = make_build_env () in
  (* Load from a single repo *)
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  (* Build astring as a sanity check *)
  let target = OpamPackage.of_string "astring.0.8.5" in
  Printf.printf "Building %s via of_repositories...\n%!"
    (OpamPackage.to_string target);
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas target
    |> ok_or_fail "build_tool astring"
  in
  Printf.printf "astring built: %d layers\n%!" (List.length tool.builds);
  Alcotest.(check bool) "has layers" true (List.length tool.builds > 0)

let () =
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_tools_pinned"
      [ ( "Pinned source",
          [ Alcotest.test_case "odoc-parser from source" `Slow
              test_build_odoc_parser_from_source;
            Alcotest.test_case "odoc-driver from source" `Slow
              test_build_odoc_driver_from_source;
          ] );
        ( "Multi-repo",
          [ Alcotest.test_case "build with of_repositories" `Slow
              test_build_with_multi_repo;
          ] );
      ]
