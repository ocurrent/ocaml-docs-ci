(** batch command: solve, build, and optionally generate docs *)

open Cmdliner
module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
module Layer = Day11_layer.Layer
type build = Build.t
type tool = Tool.t



let cleanup_stale_mounts () =
  (* Unmount and remove any leaked day11_run_* overlay mounts from
     previous runs that were killed without cleanup *)
  let tmp = Filename.get_temp_dir_name () in
  let entries = try Sys.readdir tmp |> Array.to_list with _ -> [] in
  let stale = List.filter (fun name ->
    String.length name > 10 &&
    String.sub name 0 10 = "day11_run_"
  ) entries in
  if stale <> [] then begin
    Printf.printf "Cleaning up %d stale temp dirs...\n%!" (List.length stale);
    List.iter (fun name ->
      let merged = Filename.concat (Filename.concat tmp name) "merged" in
      ignore (Sys.command (Printf.sprintf "sudo umount %s 2>/dev/null" merged));
    ) stale;
    ignore (Sys.command (Printf.sprintf "sudo rm -rf %s"
      (String.concat " " (List.map (fun name ->
        Filename.concat tmp name) stale))))
  end

let run profile_name profile_dir np cores_per_build overcommit
    solve_only dry_run rebuild_failed rebuild_base fake_build target_override =
  Common.with_eio @@ fun ~sw env ->
  cleanup_stale_mounts ();
  (* Build the NUMA-aware cpu slot pool if requested. [np] is adjusted
     down to match so the outer concurrency never exceeds what the pool
     can serve. *)
  let cpu_slots = match cores_per_build with
    | None | Some 0 -> None
    | Some n ->
      let pool =
        Day11_runner.Cpu_slots.auto ~cores_per_build:n ~overcommit () in
      Printf.printf "CPU pool: %s\n%!"
        (Day11_runner.Cpu_slots.describe pool);
      Some pool
  in
  let np = match cpu_slots with
    | Some pool ->
      let n = Day11_runner.Cpu_slots.n_slots pool in
      if n < np then begin
        Printf.printf "Reducing -j from %d to %d to match cpu pool\n%!"
          np n; n
      end else np
    | None -> np
  in
  let profile, paths = match Common.load_profile ~profile_dir ~name:profile_name with
    | Ok x -> x | Error (`Msg e) -> Printf.eprintf "Error: %s\n" e; exit 1
  in
  Common.ensure_paths paths;
  (* Warn if base image digest is stale or not pinned *)
  if Day11_batch.Profile.base_image_stale profile then
    Printf.printf "WARNING: Base image digest is %s. Run 'day11 profile refresh-base --name %s' to update.\n%!"
      (match profile.base_image_digest with
       | None -> "not pinned"
       | Some _ -> "more than 30 days old")
      profile_name;
  let ctx = Day11_batch.Profile_ctx.load profile
    ~cache_dir:paths.cache_dir in
  let ctx = match cpu_slots with
    | Some pool -> Day11_batch.Profile_ctx.with_cpu_slots ctx pool
    | None -> ctx in
  let with_doc = profile.with_doc in
  let extra_pins = profile.extra_pins in
  let jtw_repo = if profile.with_jtw then profile.jtw_repo else None in
  let small_universe, all_versions, target, profile_packages =
    match target_override with
    | Some t ->
      (* CLI target overrides the profile's target mode *)
      (false, false, Some t, None)
    | None ->
      let open Day11_batch.Profile in
      let tm = profile.target_mode in
      let av = match tm.versions with
        | All_versions -> true
        | Latest_n _ -> false
      in
      let pkgs = match tm.names with
        | Names names -> Some names
        | All_names -> None
      in
      (false, av, None, pkgs)
  in
  (* Convenience aliases from ctx — avoids churn in the (still large)
     body below. All profile-derived state that used to be bound
     individually now comes from [ctx]. *)
  let git_packages = ctx.git_packages in
  let repos_with_shas = ctx.repos_with_shas in
  let _opam_env = ctx.opam_env in
  let ocaml_version = ctx.ocaml_version in
  let os_dir = ctx.os_dir in
  let cache_dir = ctx.cache_dir in
  let targets = match profile_packages with
    | Some names ->
      (* Profile declares an explicit list of package names. Resolve
         each to its latest non-avoided version in the solver repos
         — mirrors how ocaml-docs-ci uses [Track.v] with the names
         as a filter. *)
      Printf.printf "Using profile packages: %d names\n%!"
        (List.length names);
      List.filter_map (fun name ->
        match Day11_batch.Targets.pick_latest_version git_packages name with
        | v :: _ -> Some v
        | [] ->
          Printf.eprintf "warning: no versions found for %s\n%!" name;
          None) names
    | None ->
      Day11_batch.Targets.resolve ~small:small_universe ~all_versions
        git_packages target
  in
  Printf.printf "Targets: %d packages\n%!" (List.length targets);
  (* Snapshot — deterministic dir keyed by repo HEADs *)
  let snapshot = Day11_batch.Snapshot.current profile in
  let snapshot_dir = Fpath.(paths.snapshots_base / snapshot.key) in
  ignore (Bos.OS.Dir.create ~path:true snapshot_dir);
  ignore (Day11_batch.Snapshot.save snapshot_dir snapshot);
  Printf.printf "Snapshot: %s\n%!" snapshot.key;
  (* Start run log *)
  Day11_lib.Run_log.set_log_base_dir (Fpath.to_string snapshot_dir);
  let run_log = Day11_lib.Run_log.start_run () in
  Day11_lib.Run_log.write_plan run_log ~repos_with_shas
    ~n_targets:(List.length targets)
    ~ocaml_version:(Option.map OpamPackage.to_string ocaml_version)
    ~with_doc ~all_versions ~small_universe;
  (match ocaml_version with
   | Some v -> Printf.printf "Compiler: %s\n%!" (OpamPackage.to_string v)
   | None -> ());
  (* Solve — load cached solutions where possible *)
  let solutions_dir = Day11_batch.Snapshot.solutions_dir snapshot_dir in
  Bos.OS.Dir.create ~path:true solutions_dir |> ignore;
  let cached = ref 0 in
  let need_solve = List.filter (fun target ->
    let cache_file = Fpath.(solutions_dir /
      (OpamPackage.to_string target ^ ".json")) in
    if Sys.file_exists (Fpath.to_string cache_file) then begin
      incr cached; false
    end else true
  ) targets in
  Printf.printf "Solving: %d cached, %d need solving (%d workers)...\n%!"
    !cached (List.length need_solve) np;
  (* Latest-version mode: profile lists package names, day11 picked
     the latest version per name. The version is just a hint — let
     the solver pick a different version when the universe forces it
     (e.g. an oxcaml [+ox] variant). all-versions / small-universe
     modes still pin because each (name, version) is a separate
     intentional target. See {!Day11_solver.Solve.solve}. *)
  let pin_target = profile_packages = None || all_versions in
  (* Convert [profile.pinned_versions] strings into [OpamPackage.t]s
     for the solver's [~constraints]. Each entry pins one package
     name to an exact version, propagating the pinned-flavour
     preference through the transitive deps — same shape as
     [oi]'s [x-oi-toolchain-roots] mechanism. *)
  let pinned_constraints =
    List.filter_map (fun s ->
      try Some (OpamPackage.of_string s)
      with _ ->
        Printf.eprintf "warning: ignoring malformed pinned_versions entry %S\n%!" s;
        None
    ) profile.pinned_versions
  in
  let results = Day11_solver_pool.Solver_pool.solve_many ~sw env
    ?ocaml_version ~np ~repos:repos_with_shas
    ~constraints:pinned_constraints
    ~pin_target need_solve in
  (* Retry failed solves with older versions (useful for overlays that
     pin transitive deps to specific versions) *)
  let results, targets =
    if not small_universe then (results, targets)
    else
    let new_results = ref [] in
    let new_targets = ref [] in
    let solved_names = Hashtbl.create 16 in
    (* First pass: collect successes *)
    List.iter (fun (target, result) ->
      match result with
      | Ok _ ->
        new_results := (target, result) :: !new_results;
        Hashtbl.replace solved_names
          (OpamPackage.name target) target
      | Error _ -> ()
    ) results;
    (* Second pass: retry failures *)
    List.iter (fun (target, result) ->
      match result with
      | Ok _ -> ()
      | Error _ ->
        let name = OpamPackage.Name.to_string (OpamPackage.name target) in
        let candidates = Day11_batch.Targets.pick_latest_version git_packages name in
        let older = List.filter (fun pkg ->
          OpamPackage.Version.compare (OpamPackage.version pkg)
            (OpamPackage.version target) < 0
        ) candidates in
        if older = [] then
          new_results := (target, result) :: !new_results
        else begin
          Printf.printf "  Retrying %s with older versions...\n%!" name;
          let retries = Day11_solver_pool.Solver_pool.solve_many ~sw env
            ?ocaml_version ~np:1 ~repos:repos_with_shas
            ~constraints:pinned_constraints older in
          match List.find_opt (fun (_, r) -> Result.is_ok r) retries with
          | Some hit ->
            Printf.printf "  %s -> %s\n%!"
              name (OpamPackage.to_string (fst hit));
            new_results := hit :: !new_results;
            Hashtbl.replace solved_names
              (OpamPackage.name target) (fst hit)
          | None ->
            new_results := (target, result) :: !new_results
        end
    ) results;
    (* Update targets to use the versions that actually solved *)
    List.iter (fun target ->
      match Hashtbl.find_opt solved_names (OpamPackage.name target) with
      | Some pkg -> new_targets := pkg :: !new_targets
      | None -> new_targets := target :: !new_targets
    ) targets;
    (List.rev !new_results, List.rev !new_targets)
  in
  (* Save new solutions *)
  List.iter (fun (target, result) ->
    let entry = match result with
      | Ok result ->
        Day11_batch.Incremental_solver.Cached_solution {
          package = target; result; cache_key = None }
      | Error (msg, examined) ->
        Day11_batch.Incremental_solver.Cached_failure {
          package = target; error = msg; examined; cache_key = None }
    in
    ignore (Day11_batch.Incremental_solver.save
      Fpath.(solutions_dir / (OpamPackage.to_string target ^ ".json"))
      entry)
  ) results;
  (* Load all solutions (cached + new) *)
  let solutions = List.filter_map (fun target ->
    let cache_file = Fpath.(solutions_dir /
      (OpamPackage.to_string target ^ ".json")) in
    match Day11_batch.Incremental_solver.load cache_file with
    | Ok (Day11_batch.Incremental_solver.Cached_solution { result; _ }) ->
      Some (target, result)
    | _ -> None
  ) targets in
  (* Extract build_deps for consumers that don't need doc_deps *)
  let build_solutions = List.map (fun (t, r) ->
    (t, (r : Day11_solution.Solve_result.t).build_deps)) solutions in
  let n_solved = List.length solutions in
  let n_failed = List.length targets - n_solved in
  Printf.printf "Solved: %d/%d (%d failed)\n%!" n_solved (List.length targets) n_failed;
  Day11_lib.Run_log.write_solve run_log ~n_solved ~n_failed;
  if solve_only then begin
    Printf.printf "Solutions cached in %s\n%!" (Fpath.to_string solutions_dir);
    0
  end else
  let patches = ctx.patches in
  (* Delete base image early if --rebuild-base, before loading *)
  if rebuild_base then begin
    (* The base layer now lives under [os_dir] (os_dir/base), so removing
       [os_dir] clears the base image and all build layers together. *)
    Printf.printf "Deleting base image and all build layers for rebuild...\n%!";
    (* Root-owned files inside — go straight to sudo rm -rf. *)
    ignore (Sys.command
      (Printf.sprintf "sudo rm -rf %s" (Fpath.to_string os_dir)))
  end;
  (* Build DAG — no Eio needed. Uses the ctx's hash cache so that later
     doc-tool planning sees the same hashes. *)
  let cache = ctx.hash_cache in
  (* Triples carry doc_deps so Dag uses doc-deps-keyed universe;
     [build_solutions] (just build_deps) keeps working for Blessing
     and JTW. *)
  let dag_solutions = List.map (fun (t, r) ->
    (t, (r : Day11_solution.Solve_result.t).build_deps,
     r.Day11_solution.Solve_result.doc_deps)) solutions in
  let nodes = Day11_opam_build.Dag.build_dag cache
    ~base_hash:ctx.base.hash dag_solutions in
  Printf.printf "DAG: %d unique build nodes\n%!" (List.length nodes);
  (* Delete failed layers if --rebuild-failed *)
  if rebuild_failed then begin
    let root_deleted = ref 0 in
    let cascade_deleted = ref 0 in
    List.iter (fun (node : Day11_opam_layer.Build.t) ->
      let layer = Build.layer ~os_dir node in
      if Layer.exists env layer then
        match Day11_layer.Meta.load env (Layer.meta_path layer) with
        | Ok { exit_status; failed_dep; _ } when exit_status <> 0 ->
          ignore (Bos.OS.Path.delete ~recurse:true (Layer.dir layer));
          if failed_dep = None then incr root_deleted
          else incr cascade_deleted
        | _ -> ()
    ) nodes;
    if !root_deleted + !cascade_deleted > 0 then
      Printf.printf "Deleted %d root failures + %d cascade failures for rebuild\n%!"
        !root_deleted !cascade_deleted
  end;
  (* Check which layers already exist *)
  let n_cached = List.length (List.filter (fun (node : Day11_opam_layer.Build.t) ->
    Layer.exists env (Build.layer ~os_dir node)
  ) nodes) in
  let n_need_build = List.length nodes - n_cached in
  Printf.printf "Layers: %d cached, %d need building\n%!" n_cached n_need_build;
  Day11_lib.Run_log.write_dag run_log ~n_build:(List.length nodes)
    ~n_cached ~n_need_build;
  if dry_run then begin
    if n_need_build > 0 then begin
      Printf.printf "\nLayers to build:\n";
      List.iter (fun (node : Day11_opam_layer.Build.t) ->
        if not (Layer.exists env (Build.layer ~os_dir node)) then
          Printf.printf "  %s (%d deps)\n"
            (OpamPackage.to_string node.pkg) (List.length node.deps)
      ) nodes
    end;
    0
  end else begin
  (* === Build phase (needs base image, containers) === *)
  (* [rebuild_base] deletes the cached opam-build binary too — it
     gets rebuilt from source as part of [ensure_base]. *)
  if rebuild_base then begin
    let bin = Fpath.(cache_dir / "opam-build-bin") in
    ignore (Bos.OS.File.delete bin)
  end;
  (* Build opam-build and the base image in one step. *)
  let ctx = match Day11_batch.Profile_ctx.ensure_base ~sw env ctx with
    | Ok c -> c
    | Error (`Msg e) ->
      Printf.eprintf "Base image build failed: %s\n%!" e; exit 1
  in
  let benv = ctx.benv in
  Day11_opam_build.Types.ensure_dirs benv;
  (* No full merged-repo mount at [/home/opam/.opam/repo/default]: each
     build mounts a per-package slice there (Container_backend.build,
     from [~opam_repositories]) and only needs its own package — deps
     are pre-installed in the overlay. Mounting the whole ~18k-package
     repo on top shadowed that slice and made opam scan all of it on
     every switch-state load (multi-second stall per build). *)
  let opam_build_repo = Option.map Fpath.v profile.opam_build_repo in
  let base_mounts =
    (match Day11_opam_build.Base.opam_build_mount ~cache_dir
             ?opam_build_repo () with
     | Some m -> [ m ] | None -> [])
  in
  (* Bless *)
  let blessing_maps = Day11_batch.Blessing.compute_blessings build_solutions in
  (* Build function for the unified DAG *)
  let packages_dir = Day11_batch.Snapshot.packages_dir snapshot_dir in
  ignore (Bos.OS.Dir.create ~path:true packages_dir);
  let fake_strategy pkg =
    let pkg_str = OpamPackage.to_string pkg in
    { Day11_opam_build.Types.cmd =
        Printf.sprintf "echo 'fake-build %s'" pkg_str;
      cleanup = Day11_opam_build.Build_layer.opam_build_cleanup }
  in
  (* Shared writer for build outcomes, symlinks, and run-log lines. *)
  let recorder = Day11_batch.Recorder.create
    ~env ~os_dir ~packages_dir ~blessing_maps ~run_log in
  let snapshot_repos = List.map Fpath.v profile.opam_repositories in
  let build_one (node : Day11_opam_layer.Build.t) =
    let strategy =
      if fake_build then Some (fake_strategy node.pkg)
      else None
    in
    match Day11_opam_build.Build_layer.build ~sw env benv ?patches
            ~opam_repositories:snapshot_repos
            ~mounts:base_mounts ~snapshot_repos node ?strategy () with
    | Day11_opam_build.Types.Success _ -> true
    | _ -> false
  in
  (* Build + Docs (unified pipeline when --with-doc) *)
  let doc_outcomes = ref [] in
  let doc_outcomes_lock = Mutex.create () in
  if with_doc then begin
    let on_pkg_complete node ~cached:_ ~success =
      Day11_batch.Recorder.record_build recorder node ~success
    in
    let on_doc_complete (node : Day11_opam_layer.Build.t)
        ~cached:_ ~blessed ~universe:_ ~success =
      let layer_dir = Day11_opam_layer.Build.dir ~os_dir node in
      let log_file =
        let p = Fpath.(layer_dir / "build.log") in
        if Sys.file_exists (Fpath.to_string p) then Some p else None
      in
      let outcome : Day11_batch.Summary.doc_outcome = {
        pkg = node.pkg;
        success;
        layer_hash = node.hash;
        log_file;
        blessed;
      } in
      Mutex.lock doc_outcomes_lock;
      doc_outcomes := outcome :: !doc_outcomes;
      Mutex.unlock doc_outcomes_lock
    in
    Day11_doc.Generate.build_tools_and_run ~sw env ctx ~np
      ~mounts:base_mounts ~build_one
      ~on_pkg_complete ~on_doc_complete ~snapshot_dir ~run_log
      ~nodes ~solutions ~blessing_maps ()
  end
  else begin
    (* Build only — no docs *)
    let is_cached node =
      let layer = Build.layer ~os_dir node in
      if not (Layer.exists env layer) then
        Day11_opam_build.Dag_executor.Not_cached
      else begin
        Day11_layer.Last_used.touch env (Layer.dir layer);
        match Day11_layer.Meta.load env (Layer.meta_path layer) with
        | Ok meta ->
          if meta.exit_status = 0 then Day11_opam_build.Dag_executor.Cached_ok
          else Day11_opam_build.Dag_executor.Cached_fail
        | Error _ -> Day11_opam_build.Dag_executor.Cached_fail
      end
    in
    let cascaded_set : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    Day11_opam_build.Dag_executor.execute env ~np ~is_cached
      ~on_complete:(fun ~stats ~cached:_ node success ->
        let open Day11_opam_build.Dag_executor in
        if Hashtbl.mem cascaded_set node.hash then
          ()
        else begin
          Day11_batch.Recorder.record_build recorder node ~success;
          if not success then
            Printf.printf "[%d/%d, %d ok, %d failed, %d cascade] FAIL: %s\n%!"
              stats.completed stats.total stats.ok stats.failed
              stats.cascaded (OpamPackage.to_string node.pkg)
          else if stats.completed mod 100 = 0 then
            Printf.printf "[%d/%d, %d ok, %d failed, %d cascade] %s\n%!"
              stats.completed stats.total stats.ok stats.failed
              stats.cascaded (OpamPackage.to_string node.pkg)
        end)
      ~on_cascade:(fun ~failed ~failed_dep ->
        Hashtbl.replace cascaded_set failed.hash ();
        Day11_batch.Recorder.record_cascade recorder ~failed ~failed_dep)
      nodes build_one
  end;
  (* JTW *)
  (match jtw_repo with
   | Some dir ->
     let output = Fpath.to_string Fpath.(cache_dir / "jtw-output") in
     Day11_jtw.Build_tools.build_and_run ~sw env benv ~np ~os_dir
       ~packages:git_packages ~repos:repos_with_shas ~mounts:base_mounts
       ~extra_repo_dirs:extra_pins ~repo_dir:dir ~output
       ~nodes ~solutions:build_solutions
   | None -> ());
  (* Write final summary via Summary module *)
  Day11_lib.Run_log.close_build_log ();
  let results : Day11_batch.Summary.results = {
    builds = Day11_batch.Recorder.outcomes recorder;
    docs = !doc_outcomes;
    targets;
  } in
  ignore (Day11_batch.Summary.finish ~snapshot_dir ~packages_dir
    ~run_info:run_log results);
  0
  end

let solve_only_term =
  let doc = "Solve only — cache solutions and exit without building" in
  Arg.(value & flag & info [ "solve-only" ] ~doc)

let dry_run_term =
  let doc = "Show what would be built without actually building" in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let rebuild_failed_term =
  let doc = "Delete failed layers and rebuild them" in
  Arg.(value & flag & info [ "rebuild-failed" ] ~doc)

let rebuild_base_term =
  let doc = "Delete and rebuild the base image (use when repos or opam-build change)" in
  Arg.(value & flag & info [ "rebuild-base" ] ~doc)

let fake_build_term =
  let doc = "Replace opam-build with a trivial echo command (for testing)" in
  Arg.(value & flag & info [ "fake-build" ] ~doc)

let target_term =
  let doc = "Optional target package (overrides profile's target mode)" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"TARGET" ~doc)

let cores_per_build_term =
  let doc = "Cores per container. Enables cgroup cpuset pinning and \
             NUMA-local memory allocation when the host has multiple \
             NUMA nodes. Host CPUs are divided into slots of this \
             size, and [-j] is capped to the slot count. 0 (default) \
             disables pinning — containers see all host CPUs." in
  Arg.(value & opt (some int) None
       & info [ "cores-per-build" ] ~docv:"N" ~doc)

let overcommit_term =
  let doc = "Multiplier on the strict CPU-bounded slot count. 1.0 \
             (default) means each build has exclusive use of its \
             cpuset; 1.5 lets two builds share one cpuset 50% of the \
             time; 2.0 means every cpuset is doubled. Only takes \
             effect when [--cores-per-build] is set." in
  Arg.(value & opt float 1.0
       & info [ "overcommit" ] ~docv:"FACTOR" ~doc)

let cmd =
  let info = Cmd.info "batch" ~doc:"Solve, build, and document packages" in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term
    $ Common.np_term $ cores_per_build_term $ overcommit_term
    $ solve_only_term $ dry_run_term
    $ rebuild_failed_term $ rebuild_base_term $ fake_build_term
    $ target_term) in
  Cmd.v info term
