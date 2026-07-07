(** status command: show build status overview *)

open Cmdliner

let run profile_name profile_dir =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) -> Printf.eprintf "Error: %s\n%!" e; 1
  | Ok (_profile, paths) ->
  match Common.latest_snapshot_dir paths with
  | None -> Printf.printf "No snapshots found\n"; 1
  | Some snapshot_dir ->
  match Day11_lib.Status_index.read ~dir:snapshot_dir with
  | None ->
    (match Day11_batch.Live_view.load_latest ~snapshot_dir with
     | None ->
       Printf.printf "No status.json found, and no run in progress\n";
       1
     | Some lv ->
       Printf.printf "Run: %s (in progress — no status.json yet)\n"
         lv.run_id;
       Printf.printf "Progress: %s\n"
         (Day11_batch.Live_view.format_progress lv.progress);
       Printf.printf "Outcomes so far: %d ok, %d fail, %d cascade\n"
         lv.counts.ok lv.counts.fail lv.counts.cascade;
       0)
  | Some status ->
    Printf.printf "Run: %s (generated %s)\n" status.run_id status.generated;
    Printf.printf "\nBlessed:\n";
    List.iter (fun (cat, n) ->
      Printf.printf "  %-20s %d\n" cat n
    ) status.blessed_totals;
    Printf.printf "\nNon-blessed:\n";
    List.iter (fun (cat, n) ->
      Printf.printf "  %-20s %d\n" cat n
    ) status.non_blessed_totals;
    0

let cmd =
  let info = Cmd.info "status" ~doc:"Show build status overview" in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
