(** report command: generate daily build summary *)

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
      | Some snapshot_dir -> (
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
          let run_pkg_status json =
            let open Yojson.Safe.Util in
            let layers = json |> member "layers" |> to_assoc in
            let pkg_status : (string, string) Hashtbl.t = Hashtbl.create 256 in
            List.iter
              (fun (_hash, entry) ->
                let pkg = entry |> member "package" |> to_string in
                let status = entry |> member "status" |> to_string in
                match Hashtbl.find_opt pkg_status pkg with
                | Some "ok" -> ()
                | _ -> Hashtbl.replace pkg_status pkg status)
              layers;
            pkg_status
          in
          let runs =
            match Bos.OS.Dir.contents runs_dir with
            | Ok files ->
                files
                |> List.filter (fun f -> Fpath.has_ext ".json" f)
                |> List.sort (fun a b ->
                    compare (Fpath.to_string b) (Fpath.to_string a))
            | Error _ -> []
          in
          match runs with
          | [] -> (
              (* No finished run summaries on disk. If a run is in progress,
       emit a minimal live report so the operator can see something
       rather than "No runs found". *)
              match Day11_batch.Live_view.load_latest ~snapshot_dir with
              | None ->
                  Printf.printf "No runs found.\n";
                  1
              | Some lv ->
                  Printf.printf "# Live Build Report — run %s (in progress)\n\n"
                    lv.run_id;
                  Printf.printf "Progress: %s\n\n"
                    (Day11_batch.Live_view.format_progress lv.progress);
                  Printf.printf "## Outcomes so far\n\n";
                  Printf.printf "| Status | Count |\n|--------|-------|\n";
                  Printf.printf "| ok | %d |\n" lv.counts.ok;
                  Printf.printf "| fail | %d |\n" lv.counts.fail;
                  Printf.printf "| cascade | %d |\n\n" lv.counts.cascade;
                  let failed =
                    Hashtbl.fold
                      (fun pkg s acc -> if s = "fail" then pkg :: acc else acc)
                      lv.pkg_status []
                  in
                  let failed = List.sort compare failed in
                  if failed <> [] then (
                    Printf.printf "## Failing packages (%d)\n\n"
                      (List.length failed);
                    List.iter
                      (fun p -> Printf.printf "- `%s`\n" p)
                      (List.filteri (fun i _ -> i < 50) failed);
                    if List.length failed > 50 then
                      Printf.printf "\n_...and %d more_\n"
                        (List.length failed - 50));
                  0)
          | latest_file :: rest -> (
              match load_run latest_file with
              | None ->
                  Printf.printf "Cannot load latest run.\n";
                  1
              | Some latest_json ->
                  let open Yojson.Safe.Util in
                  let ts = latest_json |> member "timestamp" |> to_string in
                  let repos = latest_json |> member "repos" |> to_list in
                  let latest_ps = run_pkg_status latest_json in
                  let n_ok =
                    Hashtbl.fold
                      (fun _ s n -> if s = "ok" then n + 1 else n)
                      latest_ps 0
                  in
                  let n_fail =
                    Hashtbl.fold
                      (fun _ s n -> if s = "fail" then n + 1 else n)
                      latest_ps 0
                  in
                  let n_cascade =
                    Hashtbl.fold
                      (fun _ s n -> if s = "cascade" then n + 1 else n)
                      latest_ps 0
                  in
                  (* Generate report *)
                  let buf = Buffer.create 4096 in
                  let pr fmt = Printf.bprintf buf fmt in
                  pr "# Daily Build Report — %s\n\n" (String.sub ts 0 10);
                  pr "## Repositories\n\n";
                  List.iter
                    (fun r ->
                      let path = r |> member "path" |> to_string in
                      let commit = r |> member "commit" |> to_string in
                      pr "- `%s` @ `%s`\n" (Filename.basename path)
                        (String.sub commit 0 12))
                    repos;
                  pr "\n## Build Summary\n\n";
                  pr "| Metric | Count |\n";
                  pr "|--------|-------|\n";
                  pr "| Succeeded | %d |\n" n_ok;
                  pr "| Failed (root) | %d |\n" n_fail;
                  pr "| Failed (cascade) | %d |\n" n_cascade;
                  pr "| Total | %d |\n" (n_ok + n_fail + n_cascade);
                  pr "\n";
                  (* Diff with previous *)
                  let prev_opt =
                    match rest with
                    | prev_file :: _ -> load_run prev_file
                    | [] -> None
                  in
                  (match prev_opt with
                  | Some prev_json ->
                      let prev_ps = run_pkg_status prev_json in
                      let prev_ts =
                        prev_json |> member "timestamp" |> to_string
                      in
                      let prev_ok =
                        Hashtbl.fold
                          (fun _ s n -> if s = "ok" then n + 1 else n)
                          prev_ps 0
                      in
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
                      pr "## Changes (vs %s)\n\n" (String.sub prev_ts 0 10);
                      pr "- **Previous:** %d succeeded\n" prev_ok;
                      pr "- **Current:** %d succeeded\n" n_ok;
                      pr "- **Net change:** %+d\n\n" (n_ok - prev_ok);
                      if fixed <> [] then (
                        pr "### Newly passing (%d)\n\n" (List.length fixed);
                        List.iter (fun p -> pr "- `%s`\n" p) fixed;
                        pr "\n");
                      if regressed <> [] then (
                        pr "### Newly failing (%d)\n\n" (List.length regressed);
                        List.iter (fun p -> pr "- `%s`\n" p) regressed;
                        pr "\n")
                  | None -> pr "## Changes\n\nNo previous run to compare.\n\n");
                  (* Top blockers -- compute cascade block counts from failed_dep *)
                  pr "## Top Blockers\n\n";
                  let layers = latest_json |> member "layers" |> to_assoc in
                  (* blocked_by.(dep) = list of packages blocked by dep *)
                  let blocked_by : (string, string list) Hashtbl.t =
                    Hashtbl.create 64
                  in
                  List.iter
                    (fun (_hash, entry) ->
                      let pkg = entry |> member "package" |> to_string in
                      let status = entry |> member "status" |> to_string in
                      if status <> "ok" then
                        match entry |> member "failed_dep" with
                        | `String dep ->
                            let prev =
                              try Hashtbl.find blocked_by dep
                              with Not_found -> []
                            in
                            if not (List.mem pkg prev) then
                              Hashtbl.replace blocked_by dep (pkg :: prev)
                        | _ -> ())
                    layers;
                  (* For "sole" count: packages whose only failed_dep is this one *)
                  let pkg_blockers : (string, string list) Hashtbl.t =
                    Hashtbl.create 64
                  in
                  List.iter
                    (fun (_hash, entry) ->
                      let pkg = entry |> member "package" |> to_string in
                      let status = entry |> member "status" |> to_string in
                      if status <> "ok" then
                        match entry |> member "failed_dep" with
                        | `String dep ->
                            let prev =
                              try Hashtbl.find pkg_blockers pkg
                              with Not_found -> []
                            in
                            if not (List.mem dep prev) then
                              Hashtbl.replace pkg_blockers pkg (dep :: prev)
                        | _ -> ())
                    layers;
                  let sole_count : (string, int) Hashtbl.t =
                    Hashtbl.create 64
                  in
                  Hashtbl.iter
                    (fun pkg deps ->
                      match deps with
                      | [ single_dep ] ->
                          ignore pkg;
                          let n =
                            try Hashtbl.find sole_count single_dep
                            with Not_found -> 0
                          in
                          Hashtbl.replace sole_count single_dep (n + 1)
                      | _ -> ())
                    pkg_blockers;
                  let fail_pkgs =
                    Hashtbl.fold
                      (fun pkg status acc ->
                        if status = "fail" then pkg :: acc else acc)
                      latest_ps []
                  in
                  (* Sort by block count descending *)
                  let fail_pkgs =
                    List.sort
                      (fun a b ->
                        let ba =
                          try List.length (Hashtbl.find blocked_by a)
                          with Not_found -> 0
                        in
                        let bb =
                          try List.length (Hashtbl.find blocked_by b)
                          with Not_found -> 0
                        in
                        compare bb ba)
                      fail_pkgs
                  in
                  pr "| Package | Blocks | Sole |\n";
                  pr "|---------|--------|------|\n";
                  List.iteri
                    (fun i pkg ->
                      if i < 20 then
                        let blocks =
                          try List.length (Hashtbl.find blocked_by pkg)
                          with Not_found -> 0
                        in
                        let sole =
                          try Hashtbl.find sole_count pkg with Not_found -> 0
                        in
                        pr "| `%s` | %d | %d |\n" pkg blocks sole)
                    fail_pkgs;
                  if List.length fail_pkgs > 20 then pr "| ... | | |\n";
                  pr "\n*Run `day11 results` for full impact analysis.*\n";
                  (* Write report *)
                  let reports_dir = Fpath.(paths.cache_dir / "reports") in
                  Bos.OS.Dir.create ~path:true reports_dir |> ignore;
                  let date = String.sub ts 0 10 in
                  let report_file = Fpath.(reports_dir / (date ^ ".md")) in
                  let content = Buffer.contents buf in
                  ignore (Bos.OS.File.write report_file content);
                  Printf.printf "%s" content;
                  Printf.printf "\nReport saved to %s\n"
                    (Fpath.to_string report_file);
                  0)))

let cmd =
  let info = Cmd.info "report" ~doc:"Generate daily build summary report" in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
