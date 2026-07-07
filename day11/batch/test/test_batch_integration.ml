(* Integration tests for the batch library.

   Test 1-2: Pure (no containers) — solver → graph → blessing → dag_executor
   Test 3:   Container — solver → dag_executor with real builds
   Test 4:   Incremental solver reuse across opam-repo SHAs

   Run with: DAY11_INTEGRATION=true dune exec day11/batch/test/test_batch_integration.exe *)

open Day11_batch
open Day11_opam_build
open Day11_test_util.Test_util

let arch = "x86_64"
let os_distribution = "debian"

let pkg s = OpamPackage.of_string s

let solve_package git_packages opam_env target_str =
  let target = pkg target_str in
  match Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env target with
  | Ok result -> (target, result.Day11_solution.Solve_result.build_deps)
  | Error (diag, _) -> Alcotest.fail ("Solve " ^ target_str ^ ": " ^ diag)

let setup_solver () =
  let opam_repository = opam_repository () in
  let git_packages, store, commit =
    Day11_opam.Git_packages.of_opam_repository opam_repository in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch ~os:"linux" ~os_distribution ~os_family:"debian"
    ~os_version:"12" () in
  (git_packages, store, commit, opam_env)

(* ── Test 1: Solver → Graph → Blessing → Dag_executor (mock builds) ── *)

let test_full_pipeline_mock () = with_eio @@ fun ~sw:_ env ->
  Printf.printf "Setting up solver...\n%!";
  let git_packages, _store, _commit, opam_env = setup_solver () in
  (* Solve two small packages to exercise blessing *)
  Printf.printf "Solving astring and fmt...\n%!";
  let sol_astring = solve_package git_packages opam_env "astring.0.8.5" in
  let sol_fmt = solve_package git_packages opam_env "fmt.0.9.0" in
  let solutions = [ sol_astring; sol_fmt ] in
  Printf.printf "  astring: %d packages\n%!"
    (OpamPackage.Map.cardinal (snd sol_astring));
  Printf.printf "  fmt: %d packages\n%!"
    (OpamPackage.Map.cardinal (snd sol_fmt));
  (* Bless *)
  let blessing_maps = Blessing.compute_blessings solutions in
  Alcotest.(check int) "2 blessing maps" 2 (List.length blessing_maps);
  (* Check that shared packages are blessed somewhere *)
  let all_blessed =
    List.fold_left (fun acc (_, map) ->
      OpamPackage.Map.fold (fun pkg b acc ->
        if b then OpamPackage.Set.add pkg acc else acc
      ) map acc
    ) OpamPackage.Set.empty blessing_maps
  in
  Printf.printf "  Blessed: %d packages\n%!"
    (OpamPackage.Set.cardinal all_blessed);
  Alcotest.(check bool) "some blessed" true
    (OpamPackage.Set.cardinal all_blessed > 0);
  (* Build DAG *)
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let base_hash = Base.hash ~image:"test" in
  let nodes =
    Dag.build_dag cache ~base_hash
      (List.map (fun (t, d) -> (t, d, d)) solutions) in
  Printf.printf "  DAG: %d nodes\n%!" (List.length nodes);
  Alcotest.(check bool) "nodes > 0" true (List.length nodes > 0);
  (* Execute with mock build — all succeed *)
  let completed = ref [] in
  Dag_executor.execute env ~np:4
    ~on_complete:(fun ~stats:_ ~cached:_ node success ->
      completed := (OpamPackage.to_string node.pkg, success) :: !completed)
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
    nodes
    (fun _node -> true);
  let completed = List.rev !completed in
  Printf.printf "  Completed: %d/%d\n%!"
    (List.length completed) (List.length nodes);
  Alcotest.(check int) "all completed"
    (List.length nodes) (List.length completed);
  (* All should succeed *)
  List.iter (fun (name, success) ->
    Alcotest.(check bool) (name ^ " success") true success
  ) completed

(* ── Test 2: Full pipeline with Summary ──────────────────────────── *)

let test_pipeline_with_summary () = with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let packages_dir = Fpath.(dir / "packages") in
  mkdir packages_dir;
  let os_dir = Fpath.(dir / "os") in
  mkdir os_dir;
  Printf.printf "Setting up solver...\n%!";
  let git_packages, _store, _commit, opam_env = setup_solver () in
  let sol_astring = solve_package git_packages opam_env "astring.0.8.5" in
  let solutions = [ sol_astring ] in
  let blessing_maps = Blessing.compute_blessings solutions in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let base_hash = Base.hash ~image:"test" in
  let nodes =
    Dag.build_dag cache ~base_hash
      (List.map (fun (t, d) -> (t, d, d)) solutions) in
  Printf.printf "  DAG: %d nodes, executing...\n%!" (List.length nodes);
  (* Build mock outcomes for summary *)
  let build_outcomes = ref [] in
  Dag_executor.execute env ~np:4
    ~on_complete:(fun ~stats:_ ~cached:_ node success ->
      let blessed =
        List.exists (fun (_, map) ->
          Blessing.is_blessed map node.pkg
        ) blessing_maps
      in
      build_outcomes :=
        { Summary.pkg = node.pkg;
          build_hash = node.hash;
          success; log_file = None; blessed }
        :: !build_outcomes)
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
    nodes
    (fun _node -> true);
  let results : Summary.results = {
    builds = List.rev !build_outcomes;
    docs = [];
    targets = [ fst sol_astring ];
  } in
  (* [status.json] is now aggregated from the run's per-node outcomes,
     not from on-disk history. *)
  Summary.write_status ~snapshot_dir:os_dir ~run_id:"test-int" results;
  let status = Day11_lib.Status_index.read
    ~dir:os_dir in
  Alcotest.(check bool) "status.json written" true (status <> None)

(* ── Test 3: Solver → Dag_executor with real container builds ──── *)

let scratch_cache_dir = Fpath.v "/tmp/day11-scratch-cache"

let test_parallel_real_builds () = with_eio @@ fun ~sw env ->
  (* Use from-scratch cache (Base.build, switch=default) *)
  let base = match Base.load_cached
    ~os_dir:Fpath.(scratch_cache_dir / "linux-x86_64")
    ~os_distribution ~os_version:"bookworm" with
    | Some b -> b
    | None ->
      Printf.printf "Skipping: no from-scratch cache\n%!";
      Alcotest.skip ()
  in
  let os_dir = Fpath.(scratch_cache_dir / "linux-x86_64") in
  Printf.printf "Setting up solver and base image...\n%!";
  let git_packages, _store, _commit, opam_env = setup_solver () in
  Printf.printf "  Base: %s\n%!" base.hash;
  (* Solve a small package *)
  let sol_astring = solve_package git_packages opam_env "astring.0.8.5" in
  let solutions = [ sol_astring ] in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let nodes = Dag.build_dag cache ~base_hash:base.hash
    (List.map (fun (t, d) -> (t, d, d)) solutions) in
  Printf.printf "  DAG: %d nodes\n%!" (List.length nodes);
  let completed_pkgs = ref [] in
  let failed_pkgs = ref [] in
  (* Base was built with uid/gid 1000 — must match *)
  let benv = Types.make_build_env ~base ~os_dir ~uid:1000 ~gid:1000 () in
  Types.ensure_dirs benv;
  Dag_executor.execute env ~np:2
    ~on_complete:(fun ~stats ~cached:_ node success ->
      Printf.printf "  [%d/%d, %d failed] %s: %s\n%!"
        stats.Day11_opam_build.Dag_executor.completed stats.total stats.failed
        (OpamPackage.to_string node.pkg)
        (if success then "OK" else "FAIL");
      if success then
        completed_pkgs := node.pkg :: !completed_pkgs
      else
        failed_pkgs := node.pkg :: !failed_pkgs)
    ~on_cascade:(fun ~failed ~failed_dep:_ ->
      Printf.printf "  CASCADE: %s\n%!" failed.hash)
    nodes
    (fun node ->
      match Build_layer.build ~sw env benv ~opam_repositories:[] node () with
      | Types.Success _bl -> true
      | _ -> false);
  Printf.printf "\n=== Results: %d succeeded, %d failed ===\n%!"
    (List.length !completed_pkgs) (List.length !failed_pkgs);
  Alcotest.(check bool) "some succeeded" true
    (List.length !completed_pkgs > 0);
  (* astring itself should have succeeded *)
  Alcotest.(check bool) "astring built" true
    (List.exists (fun p ->
      OpamPackage.to_string p = "astring.0.8.5") !completed_pkgs)

(* ── Test 4: Incremental solver reuse ────────────────────────────── *)

let test_incremental_reuse () =
  with_tmp_dir @@ fun dir ->
  Printf.printf "Setting up solver...\n%!";
  let git_packages, store, commit, opam_env = setup_solver () in
  let sha1 = Git_unix.Store.Hash.to_hex commit in
  let sha1_short = String.sub sha1 0 12 in
  let solutions_base = Fpath.(dir / "solutions") in
  mkdir solutions_base;
  let sha1_dir = Fpath.(solutions_base / sha1_short) in
  mkdir sha1_dir;
  (* Solve a few packages and cache them *)
  let targets = [ "astring.0.8.5"; "fmt.0.9.0" ] in
  Printf.printf "Solving and caching %d packages...\n%!" (List.length targets);
  List.iter (fun target_str ->
    let target = pkg target_str in
    match Day11_solver.Solve.solve
            ~packages:git_packages ~env:opam_env target with
    | Ok result ->
      let entry = Incremental_solver.Cached_solution {
        package = target; result; cache_key = None;
      } in
      (match Incremental_solver.save
               Fpath.(sha1_dir / (target_str ^ ".json")) entry with
       | Ok () -> Printf.printf "  Cached %s (examined %d)\n%!"
           target_str (OpamPackage.Name.Set.cardinal result.examined)
       | Error (`Msg e) -> Alcotest.fail e)
    | Error (diag, _examined) ->
      Alcotest.fail ("Solve " ^ target_str ^ ": " ^ diag)
  ) targets;
  (* Simulate a new SHA with no changes — all should be reused *)
  let sha2_short = "fake-new-sha1" in
  let sha2_dir = Fpath.(solutions_base / sha2_short) in
  mkdir sha2_dir;
  let changed_empty = OpamPackage.Name.Set.empty in
  let reused = Incremental_solver.reuse_solutions
    ~solutions_cache_dir:sha2_dir ~previous_dir:sha1_dir
    ~changed_packages:changed_empty ~packages:targets in
  Printf.printf "  Reused (no changes): %d/%d\n%!" reused (List.length targets);
  Alcotest.(check int) "all reused" (List.length targets) reused;
  (* Verify reused solutions load correctly *)
  List.iter (fun target_str ->
    match Incremental_solver.load Fpath.(sha2_dir / (target_str ^ ".json")) with
    | Ok (Cached_solution s) ->
      Alcotest.(check string) (target_str ^ " roundtrip")
        target_str (OpamPackage.to_string s.package)
    | _ -> Alcotest.fail ("Failed to load reused " ^ target_str)
  ) targets;
  (* Now simulate changes to a package that astring examines *)
  let sha3_short = "fake-changed1" in
  let sha3_dir = Fpath.(solutions_base / sha3_short) in
  mkdir sha3_dir;
  (* Find what astring examined and pick one *)
  let astring_entry =
    match Incremental_solver.load Fpath.(sha1_dir / "astring.0.8.5.json") with
    | Ok (Cached_solution s) -> s
    | _ -> Alcotest.fail "load astring"
  in
  let examined_name =
    OpamPackage.Name.Set.choose astring_entry.result.examined in
  Printf.printf "  Simulating change to %s...\n%!"
    (OpamPackage.Name.to_string examined_name);
  let changed_one = OpamPackage.Name.Set.singleton examined_name in
  let reused2 = Incremental_solver.reuse_solutions
    ~solutions_cache_dir:sha3_dir ~previous_dir:sha1_dir
    ~changed_packages:changed_one ~packages:targets in
  Printf.printf "  Reused (1 change): %d/%d\n%!" reused2 (List.length targets);
  (* astring should NOT be reused since its examined set overlaps *)
  Alcotest.(check bool) "not all reused" true
    (reused2 < List.length targets);
  (* Test diff_packages between HEAD~1 and HEAD *)
  Printf.printf "  Testing diff_packages...\n%!";
  (try
    let opam_repository = opam_repository () in
    let parent_sha = String.trim (
      Unix.open_process_in
        (Printf.sprintf "git -C %s rev-parse HEAD~1" opam_repository)
      |> In_channel.input_all) in
    let parent =
      Day11_opam.Git_utils.resolve_commit_in_store store (Some parent_sha) in
    let changed_real = Day11_opam.Git_packages.diff_packages
      ~store parent commit in
    Printf.printf "  Real changes (HEAD~1..HEAD): %d packages\n%!"
      (List.length changed_real)
  with _ ->
    Printf.printf "  (skipped: could not get parent)\n%!")

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_batch_integration"
      [ ( "Pure pipeline",
          [ Alcotest.test_case "solver→blessing→dag (mock)" `Slow
              test_full_pipeline_mock;
            Alcotest.test_case "pipeline with summary" `Slow
              test_pipeline_with_summary ] );
        ( "Container pipeline",
          [ Alcotest.test_case "parallel real builds" `Slow
              test_parallel_real_builds ] );
        ( "Incremental solver",
          [ Alcotest.test_case "reuse across SHAs" `Slow
              test_incremental_reuse ] );
      ]
