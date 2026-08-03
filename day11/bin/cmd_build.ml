(** build command: solve and build a single package within a profile *)

open Cmdliner
module Build = Day11_opam_layer.Build
module Layer = Day11_layer.Layer

let run profile_name profile_dir np target_str doc_output rebuild_failed =
  Common.with_eio @@ fun ~sw env ->
  let profile, paths =
    match Common.load_profile ~profile_dir ~name:profile_name with
    | Ok x -> x
    | Error (`Msg e) ->
        Printf.eprintf "Error: %s\n" e;
        exit 1
  in
  Common.ensure_paths paths;
  let ctx = Day11_batch.Profile_ctx.load profile ~cache_dir:paths.cache_dir in
  (* Aliases to keep the existing body readable. *)
  let os_dir = ctx.os_dir in
  let ocaml_version = ctx.ocaml_version in
  let repos_with_shas = ctx.repos_with_shas in
  let target = OpamPackage.of_string target_str in
  Printf.printf "Target: %s\n%!" target_str;
  (* Snapshot *)
  let snapshot = Day11_batch.Snapshot.current profile in
  let snapshot_dir = Fpath.(paths.snapshots_base / snapshot.key) in
  ignore (Bos.OS.Dir.create ~path:true snapshot_dir);
  ignore (Day11_batch.Snapshot.save snapshot_dir snapshot);
  (* Solve *)
  let solutions_dir = Day11_batch.Snapshot.solutions_dir snapshot_dir in
  ignore (Bos.OS.Dir.create ~path:true solutions_dir);
  Printf.printf "Solving...\n%!";
  let pinned_constraints =
    List.filter_map
      (fun s -> try Some (OpamPackage.of_string s) with _ -> None)
      profile.pinned_versions
  in
  let result =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ?ocaml_version ~np:1
      ~repos:repos_with_shas ~constraints:pinned_constraints [ target ]
  in
  match List.find_opt (fun (_, r) -> Result.is_ok r) result with
  | None ->
      let err =
        match result with
        | [ (_, Error (msg, _)) ] -> msg
        | _ -> "unknown error"
      in
      Printf.eprintf "Solve failed: %s\n%!" err;
      1
  | Some (target, Ok solve_result) ->
      let solution = solve_result.Day11_solution.Solve_result.build_deps in
      let doc_solution = solve_result.Day11_solution.Solve_result.doc_deps in
      Printf.printf "Solved: %d packages\n%!"
        (OpamPackage.Map.cardinal solution);
      (* Build DAG — uses the ctx's shared hash cache + base hash. *)
      let patches = ctx.patches in
      let cache = ctx.hash_cache in
      let dag_solutions = [ (target, solution, doc_solution) ] in
      let nodes =
        Day11_opam_build.Dag.build_dag cache ~base_hash:ctx.base.hash
          dag_solutions
      in
      Printf.printf "DAG: %d nodes\n%!" (List.length nodes);
      (* Delete failed layers if requested *)
      if rebuild_failed then (
        let deleted = ref 0 in
        List.iter
          (fun (node : Day11_opam_layer.Build.t) ->
            let layer = Build.layer ~os_dir node in
            if Layer.exists env layer then
              match Day11_layer.Meta.load env (Layer.meta_path layer) with
              | Ok { exit_status; _ } when exit_status <> 0 ->
                  ignore (Bos.OS.Path.delete ~recurse:true (Layer.dir layer));
                  incr deleted
              | _ -> ())
          nodes;
        if !deleted > 0 then
          Printf.printf "Deleted %d failed layers for rebuild\n%!" !deleted);
      let n_cached =
        List.length
          (List.filter
             (fun (node : Day11_opam_layer.Build.t) ->
               Layer.exists env (Build.layer ~os_dir node))
             nodes)
      in
      Printf.printf "Layers: %d cached, %d to build\n%!" n_cached
        (List.length nodes - n_cached);
      if n_cached = List.length nodes && doc_output = None then (
        Printf.printf "Everything cached, nothing to do.\n%!";
        0)
      else
        (* Build — [ensure_base] handles both the opam-build binary and
       the base image. *)
        let ctx =
          match Day11_batch.Profile_ctx.ensure_base ~sw env ctx with
          | Ok c -> c
          | Error (`Msg e) ->
              Printf.eprintf "Base image build failed: %s\n%!" e;
              exit 1
        in
        let benv = ctx.benv in
        Day11_opam_build.Types.ensure_dirs benv;
        (* Repo mount — reuse from snapshot if available, else build fresh *)
        let merged_repo_dir = Fpath.(snapshot_dir / "merged-repo") in
        (match
           Day11_opam_layer.Opam_repo.build_merged ~dest:merged_repo_dir
             profile.opam_repositories
         with
        | Ok () -> ()
        | Error (`Msg e) ->
            Printf.eprintf "Failed to build merged repo: %s\n%!" e;
            exit 1);
        let repo_mount =
          Day11_container.Mount.bind_rw
            ~src:(Fpath.to_string merged_repo_dir)
            "/home/opam/.opam/repo/default"
        in
        let opam_build_repo = Option.map Fpath.v profile.opam_build_repo in
        let base_mounts =
          [ repo_mount ]
          @
          match
            Day11_opam_build.Base.opam_build_mount ~cache_dir:paths.cache_dir
              ?opam_build_repo ()
          with
          | Some m -> [ m ]
          | None -> []
        in
        let packages_dir = Day11_batch.Snapshot.packages_dir snapshot_dir in
        ignore (Bos.OS.Dir.create ~path:true packages_dir);
        (* Build phase *)
        let open Day11_opam_build.Dag_executor in
        execute env ~np
          ~is_cached:(fun node ->
            let layer = Build.layer ~os_dir node in
            if not (Layer.exists env layer) then Not_cached
            else
              match Day11_layer.Meta.load env (Layer.meta_path layer) with
              | Ok meta when meta.exit_status = 0 ->
                  Day11_layer.Last_used.touch env (Layer.dir layer);
                  Cached_ok
              | _ -> Cached_fail)
          ~on_complete:(fun ~stats ~cached:_ node success ->
            (if success then
               let pkg_str = OpamPackage.to_string node.pkg in
               let layer_name = Day11_opam_layer.Build.dir_name node in
               ignore
                 (Day11_layer.Symlinks.ensure env ~packages_dir ~id:pkg_str
                    ~layer_name));
            if stats.completed mod 10 = 0 || not success then
              Printf.printf "[%d/%d, %d ok, %d failed] %s: %s\n%!"
                stats.completed stats.total stats.ok stats.failed
                (OpamPackage.to_string node.pkg)
                (if success then "OK" else "FAIL"))
          ~on_cascade:(fun ~failed ~failed_dep ->
            let layer = Build.layer ~os_dir failed in
            ignore (Bos.OS.Dir.create ~path:true (Layer.dir layer));
            if not (Layer.exists env layer) then
              let meta : Day11_layer.Meta.t =
                {
                  exit_status = 1;
                  parent_hashes = [];
                  uid = benv.uid;
                  gid = benv.gid;
                  base_hash = benv.base.hash;
                  disk_usage = 0;
                  timing = Day11_layer.Meta.empty_timing;
                  created_at = "";
                  failed_dep = Some (Day11_opam_layer.Build.dir_name failed_dep);
                }
              in
              ignore (Day11_layer.Meta.save env (Layer.meta_path layer) meta))
          nodes
          (fun node ->
            let opam_repos_fpath = List.map Fpath.v profile.opam_repositories in
            match
              Day11_opam_build.Build_layer.build ~sw env benv ?patches
                ~opam_repositories:opam_repos_fpath ~mounts:base_mounts node ()
            with
            | Day11_opam_build.Types.Success _ -> true
            | _ -> false);
        (* Doc phase — only if --with-doc <dir> *)
        (match doc_output with
        | None -> ()
        | Some output_dir ->
            Printf.printf "\nGenerating docs to %s...\n%!" output_dir;
            let output = Fpath.v output_dir in
            ignore (Bos.OS.Dir.create ~path:true output);
            let solutions = [ (target, solve_result) ] in
            let blessing_maps =
              [ (target, OpamPackage.Map.map (fun _ -> true) solution) ]
            in
            Day11_lib.Run_log.set_log_base_dir (Fpath.to_string snapshot_dir);
            let run_log = Day11_lib.Run_log.start_run () in
            Day11_doc.Generate.build_tools_and_run ~sw env ctx ~np
              ~mounts:base_mounts
              ~build_one:(fun node ->
                match
                  Day11_opam_build.Build_layer.build ~sw env benv ?patches
                    ~opam_repositories:
                      (List.map Fpath.v profile.opam_repositories)
                    ~mounts:base_mounts node ()
                with
                | Day11_opam_build.Types.Success _ -> true
                | _ -> false)
              ~run_log ~nodes ~solutions ~blessing_maps ();
            (* Copy HTML for packages in this solution to the output dir *)
            let html_root = Fpath.(os_dir / "html" / "p") in
            let n_copied = ref 0 in
            OpamPackage.Map.iter
              (fun pkg _ ->
                let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
                let ver =
                  OpamPackage.Version.to_string (OpamPackage.version pkg)
                in
                let src = Fpath.(html_root / name / ver) in
                if Bos.OS.Dir.exists src |> Result.get_ok then (
                  let dst = Fpath.(output / name / ver) in
                  ignore (Bos.OS.Dir.create ~path:true (Fpath.parent dst));
                  ignore
                    (Sys.command
                       (Printf.sprintf "cp -a %s %s" (Fpath.to_string src)
                          (Fpath.to_string dst)));
                  incr n_copied))
              solution;
            Printf.printf "Copied docs for %d packages to %s\n%!" !n_copied
              output_dir);
        Printf.printf "Done.\n%!";
        0
  | _ ->
      Printf.eprintf "Unexpected result\n%!";
      1

let doc_output_term =
  let doc = "Generate documentation and write HTML to DIR" in
  Arg.(value & opt (some string) None & info [ "with-doc" ] ~docv:"DIR" ~doc)

let rebuild_failed_term =
  let doc = "Delete failed layers and rebuild them" in
  Arg.(value & flag & info [ "rebuild-failed" ] ~doc)

let target_term =
  let doc = "Package to build (e.g. base.v0.17.3)" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)

let cmd =
  let doc = "Solve and build a single package" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info
    Term.(
      const run
      $ Common.profile_term
      $ Common.profile_dir_term
      $ Common.np_term
      $ target_term
      $ doc_output_term
      $ rebuild_failed_term)
