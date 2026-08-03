(* Integration test: incremental solver reuse across real opam-repo commits.

   Tests three scenarios with different change profiles:
   1. Leaf package release (rfsm.2.3) — most solutions reusable
   2. Ecosystem package release (dune 3.21.0, 17 packages) — many invalidated
   3. Compiler release (OCaml 5.4.1 + 4.14.3) — everything invalidated

   For each scenario:
   - Solve a set of target packages at the "before" commit
   - Compute diff_packages between before and after
   - Attempt incremental reuse
   - Re-solve invalidated packages at the "after" commit
   - Verify reused solutions match a fresh solve

   Run with: DAY11_INTEGRATION=true dune exec day11/batch/test/test_incremental.exe *)

open Day11_batch
open Day11_test_util.Test_util

(* Commits to test across *)
let before_leaf = "cd9fdba763a1cf1ceaa1286427354a6237c1bfe0"
let after_leaf = "2bf2bf6ea0c8867eede5e26c1c591999dd5a9ee1"
let before_dune = "7d22a58614c38be74d79ae6ef2b64994ca4ffef0"
let after_dune = "001b427da21d4d746e124eaaffab7b4134813f6d"
let before_lwt = "2bf2bf6ea0c8867eede5e26c1c591999dd5a9ee1"
let after_lwt = "7d22a58614c38be74d79ae6ef2b64994ca4ffef0"
let before_miou = "f9f7db30fd6e805d48b947df138d463a5433f4d1"
let after_miou = "38e3b080865ec919fdc7292e31dedcb8580ec6ea"

(* uring: examined by eio_main only (1/8) → expect 7/8 reused *)
let before_uring = "3c6a0f524c62627fc4bb9a8b8d9cda1d8f4d26e9"
let after_uring = "6a73dfdaa325c567c26206b9d38a2bc788fc6be8"

(* hxd: examined by dream only (1/8) → expect 7/8 reused *)
let before_hxd = "9e3a17040e4a64f2381eed061ca12fd2fde434c2"
let after_hxd = "6fdb134ad2f7372f6b3121fc5d536ec57ebe07ba"
let before_compiler = "809faa59ae6d7ced1d9cd52d11740accdb6b5e83"
let after_compiler = "d10e5a9919bb3cca75e8ad64d90d869bd5b237f0"

(* Packages to solve — diverse ecosystems *)
let target_strs =
  [
    (* lwt ecosystem *)
    "cohttp-lwt.6.2.1";
    "dream.1.0.0~alpha8";
    (* async ecosystem *)
    "cohttp-async.6.2.1";
    "core.v0.17.1";
    (* eio / standalone *)
    "eio_main.1.2";
    "zarith.1.14";
    (* simple *)
    "cmdliner.1.3.0";
    "astring.0.8.5";
  ]

let opam_env =
  Day11_opam.Opam_env.std_env ~arch:"x86_64" ~os:"linux"
    ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()

let load_packages_at_commit store commit_sha =
  let hash =
    Day11_opam.Git_utils.resolve_commit_in_store store (Some commit_sha)
  in
  Day11_opam.Git_packages.of_commit store hash

let solve_and_cache ~git_packages ~cache_dir targets =
  let solved = ref 0 in
  let failed = ref 0 in
  List.iter
    (fun target_str ->
      let target = OpamPackage.of_string target_str in
      let cache_file = Fpath.(cache_dir / (target_str ^ ".json")) in
      match
        Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env target
      with
      | Ok result -> (
          let entry =
            Incremental_solver.Cached_solution
              { package = target; result; cache_key = None }
          in
          Printf.printf "    %s: %d deps, %d examined\n%!" target_str
            (OpamPackage.Map.cardinal
               result.Day11_solution.Solve_result.build_deps)
            (OpamPackage.Name.Set.cardinal result.examined);
          match Incremental_solver.save cache_file entry with
          | Ok () -> incr solved
          | Error (`Msg e) ->
              Printf.printf "    Save error %s: %s\n%!" target_str e;
              incr failed)
      | Error (_diag, _examined) -> incr failed)
    targets;
  (!solved, !failed)

let fresh_solve ~git_packages target_str =
  let target = OpamPackage.of_string target_str in
  Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env target

let solutions_equal (s1 : Day11_solution.Solve_result.t)
    (s2 : Day11_solution.Solve_result.t) =
  OpamPackage.Map.equal OpamPackage.Set.equal s1.build_deps s2.build_deps

let test_scenario ~name ~before_sha ~after_sha () =
  with_tmp_dir @@ fun dir ->
  let opam_repository = opam_repository () in
  Printf.printf "\n=== %s ===\n%!" name;
  let store, _head =
    Day11_opam.Git_utils.get_git_repo_store_and_hash opam_repository
  in
  (* Step 1: Solve at "before" commit *)
  let before_dir = Fpath.(dir / "before") in
  mkdir before_dir;
  Printf.printf "  Loading packages at %s...\n%!" before_sha;
  let before_packages = load_packages_at_commit store before_sha in
  Printf.printf "  Solving %d targets...\n%!" (List.length target_strs);
  let solved, failed =
    solve_and_cache ~git_packages:before_packages ~cache_dir:before_dir
      target_strs
  in
  Printf.printf "  Before: %d solved, %d failed\n%!" solved failed;
  Alcotest.(check bool) "some solved" true (solved > 0);
  (* Step 2: Compute changed packages *)
  let before_hash =
    Day11_opam.Git_utils.resolve_commit_in_store store (Some before_sha)
  in
  let after_hash =
    Day11_opam.Git_utils.resolve_commit_in_store store (Some after_sha)
  in
  let changed_names =
    Day11_opam.Git_packages.diff_packages ~store before_hash after_hash
  in
  let changed_set =
    List.fold_left
      (fun s n -> OpamPackage.Name.Set.add n s)
      OpamPackage.Name.Set.empty changed_names
  in
  Printf.printf "  Changed packages: %d (%s)\n%!"
    (List.length changed_names)
    (String.concat ", "
       (List.map OpamPackage.Name.to_string
          (List.filteri (fun i _ -> i < 5) changed_names)
       @ if List.length changed_names > 5 then [ "..." ] else []));
  (* Step 3: Incremental reuse *)
  let after_dir = Fpath.(dir / "after") in
  mkdir after_dir;
  let reused =
    Incremental_solver.reuse_solutions ~solutions_cache_dir:after_dir
      ~previous_dir:before_dir ~changed_packages:changed_set
      ~packages:target_strs ()
  in
  Printf.printf "  Reused: %d/%d\n%!" reused (List.length target_strs);
  (* Step 4: Re-solve invalidated packages at "after" commit *)
  let after_packages = load_packages_at_commit store after_sha in
  let re_solved = ref 0 in
  List.iter
    (fun target_str ->
      let cache_file = Fpath.(after_dir / (target_str ^ ".json")) in
      if not (Sys.file_exists (Fpath.to_string cache_file)) then (
        Printf.printf "    Re-solving %s...\n%!" target_str;
        let target = OpamPackage.of_string target_str in
        match
          Day11_solver.Solve.solve ~packages:after_packages ~env:opam_env target
        with
        | Ok result ->
            let entry =
              Incremental_solver.Cached_solution
                { package = target; result; cache_key = None }
            in
            ignore (Incremental_solver.save cache_file entry);
            incr re_solved
        | Error _ -> incr re_solved))
    target_strs;
  Printf.printf "  Re-solved: %d\n%!" !re_solved;
  Alcotest.(check int)
    "all accounted for" (List.length target_strs) (reused + !re_solved);
  (* Step 5: Verify reused solutions match fresh solve *)
  Printf.printf "  Verifying reused solutions...\n%!";
  let mismatches = ref 0 in
  List.iter
    (fun target_str ->
      let cache_file = Fpath.(after_dir / (target_str ^ ".json")) in
      match Incremental_solver.load cache_file with
      | Ok (Cached_solution cached) -> (
          match fresh_solve ~git_packages:after_packages target_str with
          | Ok fresh_result ->
              if not (solutions_equal cached.result fresh_result) then (
                Printf.printf "    MISMATCH: %s\n%!" target_str;
                incr mismatches)
          | Error _ -> ())
      | _ -> ())
    target_strs;
  Printf.printf "  Mismatches: %d\n%!" !mismatches;
  Alcotest.(check int) "no mismatches" 0 !mismatches;
  (* Return stats for overall reporting *)
  (List.length changed_names, reused)

let test_leaf_package () =
  let changed, reused =
    test_scenario ~name:"Leaf package (rfsm.2.3)" ~before_sha:before_leaf
      ~after_sha:after_leaf ()
  in
  Printf.printf "  → %d changes, %d/%d reused\n%!" changed reused
    (List.length target_strs);
  (* rfsm is obscure enough not to be in the examined set *)
  Alcotest.(check bool)
    "most reused" true
    (reused >= List.length target_strs - 1)

let test_dune_release () =
  let changed, reused =
    test_scenario ~name:"Ecosystem package (dune 3.21.0)"
      ~before_sha:before_dune ~after_sha:after_dune ()
  in
  Printf.printf "  → %d changes, %d reused\n%!" changed reused

let test_uring_release () =
  let changed, reused =
    test_scenario ~name:"Eio-only package (uring 2.7.0)"
      ~before_sha:before_uring ~after_sha:after_uring ()
  in
  Printf.printf "  → %d changes, %d/%d reused\n%!" changed reused
    (List.length target_strs);
  (* uring only examined by eio_main — reuse all that solved *)
  Alcotest.(check bool) "most reused" true (reused >= 5)

let test_hxd_release () =
  let changed, reused =
    test_scenario ~name:"Dream-only package (hxd 0.4.0)" ~before_sha:before_hxd
      ~after_sha:after_hxd ()
  in
  Printf.printf "  → %d changes, %d/%d reused\n%!" changed reused
    (List.length target_strs);
  (* hxd only examined by dream *)
  Alcotest.(check bool) "most reused" true (reused >= 7)

let test_lwt_release () =
  let changed, reused =
    test_scenario ~name:"Mid-range package (lwt 6.0.0)" ~before_sha:before_lwt
      ~after_sha:after_lwt ()
  in
  Printf.printf "  → %d changes, %d/%d reused\n%!" changed reused
    (List.length target_strs)

let test_miou_release () =
  let changed, reused =
    test_scenario ~name:"Niche package (miou 0.5.2)" ~before_sha:before_miou
      ~after_sha:after_miou ()
  in
  Printf.printf "  → %d changes, %d/%d reused\n%!" changed reused
    (List.length target_strs)

let test_compiler_release () =
  let changed, reused =
    test_scenario ~name:"Compiler release (OCaml 5.4.1)"
      ~before_sha:before_compiler ~after_sha:after_compiler ()
  in
  Printf.printf "  → %d changes, %d reused (expect few/none reused)\n%!" changed
    reused;
  (* Compiler changes touch ocaml/ocaml-base-compiler which the solver
     examines for every package — expect no reuse *)
  Alcotest.(check bool) "few reused" true (reused <= 1)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_incremental"
      [
        ( "Incremental solver",
          [
            Alcotest.test_case "leaf package" `Slow test_leaf_package;
            Alcotest.test_case "niche (miou)" `Slow test_miou_release;
            Alcotest.test_case "eio-only (uring)" `Slow test_uring_release;
            Alcotest.test_case "dream-only (hxd)" `Slow test_hxd_release;
            Alcotest.test_case "lwt (via dune)" `Slow test_lwt_release;
            Alcotest.test_case "dune release" `Slow test_dune_release;
            Alcotest.test_case "compiler release" `Slow test_compiler_release;
          ] );
      ]
