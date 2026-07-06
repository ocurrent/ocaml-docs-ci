(* Integration test: build tools using Tools.build_tool.

   Requires: base image cache at /tmp/day11-scratch-cache,
   opam-repository at /home/jjl25/opam-repository
   Run with: DAY11_INTEGRATION=true dune exec day11/opam_build/test/test_tools.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let cache_dir = Fpath.v "/tmp/day11-scratch-cache"
let os_dir = Fpath.(cache_dir / "linux-x86_64")
let make_build_env () =
  match Base.load_cached ~os_dir
    ~os_distribution:"debian" ~os_version:"bookworm" with
  | Some base -> Types.make_build_env ~base ~os_dir ()
  | None -> Alcotest.skip ()

let test_build_astring () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      (OpamPackage.of_string "astring.0.8.5")
    |> ok_or_fail "build_tool"
  in
  Printf.printf "astring built: %d layers\n%!"
    (List.length tool.builds);
  Alcotest.(check bool) "has layers" true
    (List.length tool.builds > 0);
  let libs = Day11_opam_layer.Installed_files.scan_libs
    ~layer_dir:tool.dir in
  Alcotest.(check bool) "has astring libs" true
    (List.exists (fun f ->
       Astring.String.is_prefix ~affix:"astring" f) libs)

let test_build_odoc () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let odoc_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "odoc") in
  let odoc_pkg = match OpamPackage.Version.Map.max_binding_opt odoc_versions with
    | Some (v, _) ->
        OpamPackage.create (OpamPackage.Name.of_string "odoc") v
    | None -> Alcotest.skip ()
  in
  Printf.printf "Building %s...\n%!" (OpamPackage.to_string odoc_pkg);
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      odoc_pkg
    |> ok_or_fail "build_tool"
  in
  Printf.printf "odoc built: %d layers\n%!" (List.length tool.builds);
  Alcotest.(check bool) "has layers" true
    (List.length tool.builds > 0);
  let odoc_bin = Fpath.(tool.dir / "fs" / "home" / "opam" / ".opam"
                        / Types.switch / "bin" / "odoc") in
  Alcotest.(check bool) "odoc binary" true
    (Bos.OS.File.exists odoc_bin |> Result.get_ok)

let test_build_odoc_pinned_compiler () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let odoc_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "odoc") in
  let odoc_pkg = match OpamPackage.Version.Map.max_binding_opt odoc_versions with
    | Some (v, _) ->
        OpamPackage.create (OpamPackage.Name.of_string "odoc") v
    | None -> Alcotest.skip ()
  in
  let constraints = [ OpamPackage.of_string "ocaml-base-compiler.5.4.1" ] in
  Printf.printf "Building %s pinned to ocaml 5.4.1...\n%!"
    (OpamPackage.to_string odoc_pkg);
  let tool =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      ~constraints odoc_pkg
    |> ok_or_fail "build_tool"
  in
  Printf.printf "odoc (pinned) built: %d layers\n%!"
    (List.length tool.builds);
  let compiler_layer = List.find_opt (fun (bl : Day11_opam_layer.Build.t) ->
    Astring.String.is_prefix ~affix:"ocaml-compiler"
      (OpamPackage.to_string bl.pkg)
  ) tool.builds in
  (match compiler_layer with
   | Some bl ->
       Printf.printf "Compiler: %s\n%!"
         (OpamPackage.to_string bl.pkg);
       Alcotest.(check bool) "uses 5.4.1"
         true (Astring.String.is_infix ~affix:"5.4.1"
                 (OpamPackage.to_string bl.pkg))
   | None ->
       Printf.printf "No compiler layer found\n%!")

let test_solve_failure () = with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let benv = make_build_env () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let result =
    Tools.build_tool ~sw env benv ~packages:git_packages ~repos:repos_with_shas
      (OpamPackage.of_string "nonexistent-pkg.1.0")
  in
  Alcotest.(check bool) "returns error" true (Result.is_error result)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_tools"
      [ ( "Tools",
          [ Alcotest.test_case "build astring" `Slow test_build_astring;
            Alcotest.test_case "build odoc" `Slow test_build_odoc;
            Alcotest.test_case "build odoc pinned 5.4.1" `Slow
              test_build_odoc_pinned_compiler;
            Alcotest.test_case "solve failure" `Quick test_solve_failure;
          ] ) ]
