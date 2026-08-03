(* Integration test: build astring from scratch, including the compiler.

   Requires: Linux, runc, sudo, Docker, network access, git opam-repository.
   Run with: DAY11_INTEGRATION=true dune exec day11/opam_build/test/test_from_scratch.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let os_distribution = "debian"
let os_version = "bookworm"
let arch = "x86_64"

let test_from_scratch () =
  with_eio @@ fun ~sw env ->
  let opam_repository = opam_repository () in
  let cache_dir = Fpath.v "/tmp/day11-scratch-cache" in
  mkdir cache_dir;
  let os_dir = Fpath.(cache_dir / "linux-x86_64") in
  mkdir os_dir;
  Printf.printf "Solving astring.0.8.5...\n%!";
  let git_packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repository
  in
  let opam_env =
    Day11_opam.Opam_env.std_env ~arch ~os:"linux" ~os_distribution
      ~os_family:"debian" ~os_version:"12" ()
  in
  let solution =
    match
      Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
        (OpamPackage.of_string "astring.0.8.5")
    with
    | Ok result -> result
    | Error (diag, _) -> Alcotest.fail ("Solve failed: " ^ diag)
  in
  let pkgs =
    OpamPackage.Map.keys solution.Day11_solution.Solve_result.build_deps
  in
  Printf.printf "Solved: %d packages\n%!" (List.length pkgs);
  Printf.printf "\nBuilding base image from %s:%s...\n%!" os_distribution
    os_version;
  let base =
    Base.build ~sw env ~cache_dir ~os_distribution ~os_version ~arch ~uid:1000
      ~gid:1000 ()
    |> ok_or_fail "base build"
  in
  Printf.printf "Base: %s\n%!" (Fpath.to_string base.dir);
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Hash_cache.create ~find_opam () in
  let benv = Types.make_build_env ~base ~os_dir ~uid:1000 ~gid:1000 () in
  let _final =
    List.fold_left
      (fun (deps : Day11_opam_layer.Build.t list) pkg ->
        let pkg_str = OpamPackage.to_string pkg in
        let all_pkgs = [ pkg ] in
        let layer_hash =
          Hash_cache.layer_hash cache ~base_hash:base.hash all_pkgs
        in
        let node : Day11_opam_layer.Build.t =
          {
            hash = layer_hash;
            pkg;
            deps;
            universe = Day11_solution.Universe.dummy;
          }
        in
        Printf.printf "\n--- Building %s (layer: %s, deps: %d) ---\n%!" pkg_str
          (Day11_opam_layer.Build.dir_name node)
          (List.length deps);
        let result =
          Build_layer.build ~sw env benv ~opam_repositories:[] node ()
        in
        match result with
        | Types.Success bl ->
            Printf.printf "OK: %s → %s\n%!" pkg_str bl.hash;
            let installed =
              Day11_opam_layer.Installed_files.scan_libs
                ~layer_dir:(Day11_opam_layer.Build.dir ~os_dir:benv.os_dir bl)
            in
            Printf.printf "  Installed: %d lib files\n%!"
              (List.length installed);
            deps @ [ bl ]
        | Types.Failure name ->
            Alcotest.fail (Printf.sprintf "%s build failed: %s" pkg_str name)
        | _ -> Alcotest.fail (Printf.sprintf "%s unexpected" pkg_str))
      [] pkgs
  in
  Printf.printf "\n=== All %d packages built successfully ===\n%!"
    (List.length pkgs)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_from_scratch"
      [
        ( "From_scratch",
          [
            Alcotest.test_case "build astring from bare debian" `Slow
              test_from_scratch;
          ] );
      ]
