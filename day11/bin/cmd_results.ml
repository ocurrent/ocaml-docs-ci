(** results command: summarise build results with failure impact ranking *)

open Cmdliner

let run profile_name profile_dir =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) -> (
      match Common.latest_snapshot_dir paths with
      | None ->
          Printf.printf "No snapshots found\n";
          1
      | Some snapshot_dir ->
          (* Load solutions *)
          let solutions_dir = Fpath.(snapshot_dir / "solutions") in
          let all_solutions : (string, Day11_solution.Deps.t) Hashtbl.t =
            Hashtbl.create 4096
          in
          let n_solve_failures = ref 0 in
          (match Bos.OS.Dir.contents solutions_dir with
          | Ok commit_dirs ->
              List.iter
                (fun commit_dir ->
                  let manifest = Fpath.(commit_dir / "repos.json") in
                  (match Bos.OS.File.read manifest with
                  | Ok data -> (
                      try
                        let json = Yojson.Safe.from_string data in
                        let open Yojson.Safe.Util in
                        let repos = json |> member "repos" |> to_list in
                        Printf.printf "Solver cache: %s\n"
                          (Fpath.basename commit_dir);
                        List.iter
                          (fun r ->
                            let path = r |> member "path" |> to_string in
                            let commit = r |> member "commit" |> to_string in
                            Printf.printf "  %s @ %s\n" path
                              (String.sub commit 0 12))
                          repos;
                        (match json |> member "ocaml_version" with
                        | `String v -> Printf.printf "  compiler: %s\n" v
                        | _ -> ());
                        Printf.printf "\n"
                      with _ -> ())
                  | Error _ -> ());
                  match Bos.OS.Dir.contents commit_dir with
                  | Ok files ->
                      List.iter
                        (fun f ->
                          if
                            Fpath.has_ext ".json" f
                            && Fpath.basename f <> "repos.json"
                          then
                            match Day11_batch.Incremental_solver.load f with
                            | Ok
                                (Day11_batch.Incremental_solver.Cached_solution
                                   { package; result; _ }) ->
                                let key = OpamPackage.to_string package in
                                if not (Hashtbl.mem all_solutions key) then
                                  Hashtbl.replace all_solutions key
                                    result.build_deps
                            | Ok
                                (Day11_batch.Incremental_solver.Cached_failure _)
                              ->
                                incr n_solve_failures
                            | _ -> ())
                        files
                  | Error _ -> ())
                commit_dirs
          | Error _ -> ());
          Printf.printf "=== Solver Cache ===\n";
          Printf.printf "  Solutions cached: %d (across all solve passes)\n"
            (Hashtbl.length all_solutions);
          Printf.printf "  Solve failed:     %d\n\n" !n_solve_failures;
          (* Load runs -- each run is a subdirectory containing summary.json *)
          let runs_dir = Fpath.(snapshot_dir / "runs") in
          let load_run f =
            try
              let data =
                In_channel.with_open_text (Fpath.to_string f)
                  In_channel.input_all
              in
              Some (Yojson.Safe.from_string data)
            with _ -> None
          in
          let run_pkg_status_from_jsonl run_dir =
            let jsonl_path = Fpath.(run_dir / "build.jsonl") in
            let pkg_status : (string, string) Hashtbl.t = Hashtbl.create 256 in
            (if Sys.file_exists (Fpath.to_string jsonl_path) then
               let ic = open_in (Fpath.to_string jsonl_path) in
               Fun.protect
                 ~finally:(fun () -> close_in ic)
                 (fun () ->
                   try
                     while true do
                       let line = input_line ic in
                       if String.length line > 0 then
                         let open Yojson.Safe.Util in
                         let json = Yojson.Safe.from_string line in
                         let pkg = json |> member "pkg" |> to_string in
                         let status = json |> member "status" |> to_string in
                         match Hashtbl.find_opt pkg_status pkg with
                         | Some "ok" -> ()
                         | _ -> Hashtbl.replace pkg_status pkg status
                     done
                   with End_of_file -> ()));
            pkg_status
          in
          let runs =
            match Bos.OS.Dir.contents runs_dir with
            | Ok entries ->
                entries
                |> List.filter (fun d ->
                       Sys.file_exists
                         (Fpath.to_string Fpath.(d / "build.jsonl")))
                |> List.sort (fun a b ->
                       compare (Fpath.to_string b) (Fpath.to_string a))
            | Error _ -> []
          in
          (* If the latest run has no [summary.json], surface the live
     progress counters before continuing — the rest of the command
     reads [build.jsonl] directly, so it still works. *)
          (match runs with
          | latest :: _
            when not
                   (Sys.file_exists
                      (Fpath.to_string Fpath.(latest / "summary.json"))) -> (
              match Day11_batch.Live_view.load_latest ~snapshot_dir with
              | Some lv ->
                  Printf.printf "=== Run in progress: %s ===\n" lv.run_id;
                  Printf.printf "  %s\n\n"
                    (Day11_batch.Live_view.format_progress lv.progress)
              | None -> ())
          | _ -> ());
          (* Use latest run for build results and failure ranking *)
          (match runs with
          | latest_dir :: _ ->
              let summary_file = Fpath.(latest_dir / "summary.json") in
              let summary_opt = load_run summary_file in
              let ts =
                match summary_opt with
                | Some j -> (
                    try Yojson.Safe.Util.(j |> member "run_id" |> to_string)
                    with _ -> Fpath.basename latest_dir)
                | None -> Fpath.basename latest_dir ^ " (in progress)"
              in
              let n_target_solved =
                match summary_opt with
                | Some j -> (
                    match
                      Yojson.Safe.Util.(j |> member "targets_requested")
                    with
                    | `Int n -> Some n
                    | _ -> None)
                | None -> None
              in
              let _ = summary_opt in
              let ps = run_pkg_status_from_jsonl latest_dir in
              let n_ok =
                Hashtbl.fold (fun _ s n -> if s = "ok" then n + 1 else n) ps 0
              in
              let n_fail =
                Hashtbl.fold (fun _ s n -> if s = "fail" then n + 1 else n) ps 0
              in
              let n_cascade =
                Hashtbl.fold
                  (fun _ s n -> if s = "cascade" then n + 1 else n)
                  ps 0
              in
              Printf.printf "=== Build Results (run %s) ===\n" ts;
              (match n_target_solved with
              | Some n -> Printf.printf "  Solved (targets): %d\n" n
              | None -> ());
              Printf.printf "  Succeeded: %d\n" n_ok;
              Printf.printf "  Failed:    %d\n" n_fail;
              Printf.printf "  Cascade:   %d\n\n" n_cascade;
              (* Build failed set from this run *)
              let failed_set : (string, unit) Hashtbl.t = Hashtbl.create 64 in
              Hashtbl.iter
                (fun pkg status ->
                  if status <> "ok" then Hashtbl.replace failed_set pkg ())
                ps;
              (* Compute blocked counts from solutions *)
              let blocked_count : (string, int) Hashtbl.t = Hashtbl.create 64 in
              let sole_blocker_count : (string, int) Hashtbl.t =
                Hashtbl.create 64
              in
              Hashtbl.iter
                (fun pkg_str solution ->
                  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
                  let failed_deps = ref [] in
                  let rec walk pkg =
                    let key = OpamPackage.to_string pkg in
                    if Hashtbl.mem visited key then ()
                    else (
                      Hashtbl.replace visited key ();
                      if Hashtbl.mem failed_set key && key <> pkg_str then
                        failed_deps := key :: !failed_deps;
                      match OpamPackage.Map.find_opt pkg solution with
                      | Some deps -> OpamPackage.Set.iter walk deps
                      | None -> ())
                  in
                  let pkg = OpamPackage.of_string pkg_str in
                  walk pkg;
                  let unique_failed =
                    List.sort_uniq String.compare !failed_deps
                  in
                  List.iter
                    (fun dep ->
                      let n =
                        try Hashtbl.find blocked_count dep with Not_found -> 0
                      in
                      Hashtbl.replace blocked_count dep (n + 1);
                      if List.length unique_failed = 1 then
                        let n =
                          try Hashtbl.find sole_blocker_count dep
                          with Not_found -> 0
                        in
                        Hashtbl.replace sole_blocker_count dep (n + 1))
                    unique_failed)
                all_solutions;
              (* Root failures: failed packages with status "fail" (not cascade) *)
              let root_failures =
                Hashtbl.fold
                  (fun pkg status acc ->
                    if status = "fail" then pkg :: acc else acc)
                  ps []
              in
              let root_failures =
                List.sort
                  (fun a b ->
                    let ba =
                      try Hashtbl.find blocked_count a with Not_found -> 0
                    in
                    let bb =
                      try Hashtbl.find blocked_count b with Not_found -> 0
                    in
                    compare bb ba)
                  root_failures
              in
              if root_failures <> [] then (
                Printf.printf "Root cause failures (%d):\n"
                  (List.length root_failures);
                List.iter
                  (fun pkg ->
                    let blocks =
                      try Hashtbl.find blocked_count pkg with Not_found -> 0
                    in
                    let sole =
                      try Hashtbl.find sole_blocker_count pkg
                      with Not_found -> 0
                    in
                    if blocks > 0 then
                      Printf.printf "  %-45s (blocks %d, %d sole)\n" pkg blocks
                        sole
                    else Printf.printf "  %-45s\n" pkg)
                  root_failures);
              if n_cascade > 0 then
                Printf.printf "\nCascade: %d packages blocked by failed deps\n"
                  n_cascade
          | [] -> Printf.printf "No build runs yet\n");
          (* Run history *)
          if List.length runs > 1 then (
            Printf.printf "\n=== Run History ===\n";
            List.iter
              (fun run_dir ->
                let ts =
                  match load_run Fpath.(run_dir / "summary.json") with
                  | Some j -> (
                      try Yojson.Safe.Util.(j |> member "run_id" |> to_string)
                      with _ -> Fpath.basename run_dir)
                  | None -> Fpath.basename run_dir ^ " (in progress)"
                in
                let ps = run_pkg_status_from_jsonl run_dir in
                let n_ok =
                  Hashtbl.fold (fun _ s n -> if s = "ok" then n + 1 else n) ps 0
                in
                let n_fail =
                  Hashtbl.fold
                    (fun _ s n -> if s = "fail" then n + 1 else n)
                    ps 0
                in
                let n_cascade =
                  Hashtbl.fold
                    (fun _ s n -> if s = "cascade" then n + 1 else n)
                    ps 0
                in
                Printf.printf "  %s: %d ok, %d fail, %d cascade\n" ts n_ok
                  n_fail n_cascade)
              runs;
            (* Diff last two runs *)
            let latest_ps = run_pkg_status_from_jsonl (List.nth runs 0) in
            let prev_ps = run_pkg_status_from_jsonl (List.nth runs 1) in
            let fixed = ref [] in
            let regressed = ref [] in
            Hashtbl.iter
              (fun pkg status ->
                match Hashtbl.find_opt prev_ps pkg with
                | Some prev_status ->
                    if status = "ok" && prev_status <> "ok" then
                      fixed := pkg :: !fixed
                    else if status <> "ok" && prev_status = "ok" then
                      regressed := pkg :: !regressed
                | None -> ())
              latest_ps;
            let fixed = List.sort String.compare !fixed in
            let regressed = List.sort String.compare !regressed in
            if fixed <> [] || regressed <> [] then (
              Printf.printf "\n=== Changes (latest vs previous) ===\n";
              if fixed <> [] then (
                Printf.printf "  Fixed (%d):\n" (List.length fixed);
                List.iteri
                  (fun i p -> if i < 20 then Printf.printf "    + %s\n" p)
                  fixed;
                if List.length fixed > 20 then
                  Printf.printf "    ... and %d more\n" (List.length fixed - 20));
              if regressed <> [] then (
                Printf.printf "  Regressed (%d):\n" (List.length regressed);
                List.iteri
                  (fun i p -> if i < 20 then Printf.printf "    - %s\n" p)
                  regressed;
                if List.length regressed > 20 then
                  Printf.printf "    ... and %d more\n"
                    (List.length regressed - 20));
              let net = List.length fixed - List.length regressed in
              Printf.printf "  Net: %+d packages\n" net));
          0)

let cmd =
  let info =
    Cmd.info "results"
      ~doc:"Summarise build results with failure impact ranking"
  in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
