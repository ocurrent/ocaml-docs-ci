(* Benchmark: time actual container builds for small packages.
   Clears layer cache for each package and rebuilds from scratch.

   Requires: from-scratch cache with base image + compiler
   Run with: OPAM_REPOSITORY=... dune exec day11/benchmark/benchmark_builds.exe *)

let opam_repository =
  try Sys.getenv "OPAM_REPOSITORY"
  with Not_found -> "/home/jjl25/opam-repository"

let scratch_cache = Fpath.v "/tmp/day11-scratch-cache"

let time name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "%-40s %.3fs\n%!" name elapsed;
  result

let () =
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  Printf.printf "=== Layer build benchmark ===\n\n";
  Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let base = match Day11_opam_build.Base.load_cached
    ~os_dir:Fpath.(scratch_cache / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
    | Some b -> b
    | None -> Printf.printf "No cached base — exiting\n%!"; exit 1
  in
  let os_dir = Fpath.(scratch_cache / "linux-x86_64") in
  let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir
    ~uid:1000 ~gid:1000 () in
  Day11_opam_build.Types.ensure_dirs benv;
  let git_packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repository in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  (* Build packages that should exist in the compiler layer already.
     These are small packages that build quickly. *)
  let test_packages = [
    "astring.0.8.5";
    "fmt.0.9.0";
    "logs.0.7.0";
    "fpath.0.7.3";
    "rresult.0.7.0";
    "bos.0.2.1";
    "cmdliner.1.3.0";
    "topkg.1.1.1";
    "ptime.1.2.0";
    "uutf.1.0.3";
    "yojson.2.2.2";
    "re.1.14.0";
  ] in
  (* First solve them all to get proper DAG *)
  Printf.printf "Solving...\n%!";
  let solutions = List.filter_map (fun pkg_str ->
    match Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
            (OpamPackage.of_string pkg_str) with
    | Ok result -> Some (OpamPackage.of_string pkg_str, result.Day11_solution.Solve_result.build_deps)
    | Error _ -> Printf.printf "  FAILED: %s\n%!" pkg_str; None
  ) test_packages in
  Printf.printf "  %d solved\n\n" (List.length solutions);
  (* Build the DAG *)
  let nodes =
    Day11_opam_build.Dag.build_dag cache ~base_hash:base.hash
      (List.map (fun (t, d) -> (t, d, d)) solutions) in
  Printf.printf "DAG: %d nodes\n\n" (List.length nodes);
  (* First pass: ensure everything is cached (warm up) *)
  Printf.printf "--- Warm-up (build all, cache fills) ---\n%!";
  let _ = time "Build all (warm-up)" (fun () ->
    Day11_opam_build.Dag_executor.execute env ~np:4
      ~on_complete:(fun ~stats:_ ~cached:_ _ _ -> ())
      ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
      nodes
      (fun node ->
        match Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] node () with
        | Day11_opam_build.Types.Success _ -> true
        | _ -> false)) in
  (* Second pass: time cache hits *)
  Printf.printf "\n--- Cache hit timing ---\n%!";
  List.iter (fun pkg_str ->
    let pkg = OpamPackage.of_string pkg_str in
    match List.find_opt (fun (n : Day11_opam_layer.Build.t) ->
      OpamPackage.equal n.pkg pkg) nodes with
    | Some node ->
      ignore (time (Printf.sprintf "Build %s (cache hit)" pkg_str) (fun () ->
        Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] node ()))
    | None -> ()
  ) test_packages;
  (* Third pass: clear individual layers and time cold rebuilds *)
  Printf.printf "\n--- Cold rebuild timing ---\n%!";
  List.iter (fun pkg_str ->
    let pkg = OpamPackage.of_string pkg_str in
    match List.find_opt (fun (n : Day11_opam_layer.Build.t) ->
      OpamPackage.equal n.pkg pkg) nodes with
    | Some node ->
      let layer_dir = Day11_opam_layer.Build.dir ~os_dir node in
      (* Delete just this layer to force rebuild *)
      ignore (Day11_sys.Sudo.rm_rf ~sw env layer_dir);
      ignore (time (Printf.sprintf "Build %s (cold)" pkg_str) (fun () ->
        Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] node ()))
    | None -> ()
  ) test_packages;
  Printf.printf "\nDone.\n%!"
