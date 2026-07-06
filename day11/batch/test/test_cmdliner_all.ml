(* Integration test: solve and build ALL versions of cmdliner.

   Exercises the full batch pipeline: solver → DAG dedup → parallel builds.
   Run with: DAY11_INTEGRATION=true dune exec day11/batch/test/test_cmdliner_all.exe *)

open Day11_batch
open Day11_opam_build
open Day11_test_util.Test_util

let scratch_cache_dir = Fpath.v "/tmp/day11-scratch-cache"

let test_all_cmdliner () = with_eio @@ fun ~sw env ->
  let base = match Base.load_cached
    ~os_dir:Fpath.(scratch_cache_dir / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
    | Some b -> b
    | None ->
      Printf.printf "No from-scratch cache — skipping\n%!";
      Alcotest.skip ()
  in
  let opam_repository = opam_repository () in
  let os_dir = Fpath.(scratch_cache_dir / "linux-x86_64") in
  let benv = Types.make_build_env ~base ~os_dir ~uid:1000 ~gid:1000 () in
  Types.ensure_dirs benv;
  Printf.printf "Setting up solver...\n%!";
  let git_packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repository in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  (* Find all cmdliner versions *)
  let cmdliner_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "cmdliner") in
  let targets =
    OpamPackage.Version.Map.fold (fun v _ acc ->
      OpamPackage.create (OpamPackage.Name.of_string "cmdliner") v :: acc
    ) cmdliner_versions []
    |> List.rev
  in
  Printf.printf "Found %d cmdliner versions: %s\n%!"
    (List.length targets)
    (String.concat ", " (List.map OpamPackage.to_string targets));
  (* Solve each version *)
  let solutions = List.filter_map (fun target ->
    Printf.printf "  Solving %s... %!" (OpamPackage.to_string target);
    match Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
            target with
    | Ok result ->
      Printf.printf "%d packages\n%!" (OpamPackage.Map.cardinal result.Day11_solution.Solve_result.build_deps);
      Some (target, result.build_deps)
    | Error (diag, _) ->
      Printf.printf "FAILED: %s\n%!" diag;
      None
  ) targets in
  Printf.printf "\n%d/%d versions solved\n%!"
    (List.length solutions) (List.length targets);
  Alcotest.(check bool) "some solved" true (List.length solutions > 0);
  (* Bless *)
  let blessing_maps = Blessing.compute_blessings solutions in
  let blessed_count =
    List.fold_left (fun acc (_, map) ->
      OpamPackage.Map.fold (fun _ b acc -> if b then acc + 1 else acc) map acc
    ) 0 blessing_maps
  in
  let total_instances =
    List.fold_left (fun acc (_, map) ->
      acc + OpamPackage.Map.cardinal map
    ) 0 blessing_maps
  in
  Printf.printf "Blessed: %d/%d package instances across %d universes\n%!"
    blessed_count total_instances (List.length solutions);
  (* Build global DAG *)
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Hash_cache.create ~find_opam () in
  let nodes =
    Dag.build_dag cache ~base_hash:base.hash
      (List.map (fun (t, d) -> (t, d, d)) solutions) in
  Printf.printf "DAG: %d unique build nodes (deduplicated from %d solutions)\n%!"
    (List.length nodes) (List.length solutions);
  (* Execute with real builds *)
  let succeeded = Atomic.make 0 in
  let failed = Atomic.make 0 in
  let cascaded = Atomic.make 0 in
  let t0 = Unix.gettimeofday () in
  Dag_executor.execute env ~np:4
    ~on_complete:(fun ~stats ~cached:_ node success ->
      if success then
        Atomic.incr succeeded
      else
        Atomic.incr failed;
      if stats.Day11_opam_build.Dag_executor.completed mod 10 = 0 then
        Printf.printf "  [%d/%d] %s: %s\n%!"
          stats.completed stats.total
          (OpamPackage.to_string node.pkg)
          (if success then "OK" else "FAIL"))
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ ->
      Atomic.incr cascaded)
    nodes
    (fun node ->
      match Build_layer.build ~sw env benv ~opam_repositories:[] node () with
      | Types.Success _bl -> true
      | _ -> false);
  let elapsed = Unix.gettimeofday () -. t0 in
  let s = Atomic.get succeeded in
  let f = Atomic.get failed in
  let c = Atomic.get cascaded in
  Printf.printf "\n=== Results (%.1fs, np=4) ===\n%!" elapsed;
  Printf.printf "  %d succeeded, %d failed, %d cascaded\n%!" s f c;
  Printf.printf "  %d total nodes\n%!" (List.length nodes);
  (* Check that cmdliner nodes exist in the DAG *)
  let cmdliner_nodes =
    List.filter (fun (n : Day11_opam_layer.Build.t) ->
      String.equal "cmdliner"
        (OpamPackage.Name.to_string (OpamPackage.name n.pkg))
    ) nodes
  in
  Printf.printf "  cmdliner versions in DAG: %s\n%!"
    (String.concat ", " (List.map (fun (n : Day11_opam_layer.Build.t) ->
      OpamPackage.to_string n.pkg) cmdliner_nodes));
  Alcotest.(check bool) "some cmdliner in DAG" true
    (List.length cmdliner_nodes > 0);
  Alcotest.(check bool) "majority succeeded" true
    (s > List.length nodes / 2)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_cmdliner_all"
      [ ( "Cmdliner",
          [ Alcotest.test_case "build all versions" `Slow
              test_all_cmdliner ] ) ]
