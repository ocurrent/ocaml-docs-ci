(** snapshots command: list snapshots for a profile *)

open Cmdliner

let run profile_name profile_dir =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) ->
      let snaps = Common.snapshot_dirs_by_recency paths in
      if snaps = [] then (
        Printf.printf "No snapshots for profile '%s'\n%!" profile_name;
        0)
      else (
        Printf.printf "Snapshots for profile '%s' (%d):\n\n" profile_name
          (List.length snaps);
        List.iter
          (fun snap_dir ->
            let key = Fpath.basename snap_dir in
            let created =
              match Day11_batch.Snapshot.load snap_dir with
              | Ok s -> s.created
              | Error _ -> "?"
            in
            let n_solutions =
              let sol_dir = Day11_batch.Snapshot.solutions_dir snap_dir in
              match Bos.OS.Dir.contents sol_dir with
              | Ok files ->
                  List.length
                    (List.filter
                       (fun p ->
                         Fpath.has_ext ".json" p
                         && Fpath.basename p <> "repos.json")
                       files)
              | Error _ -> 0
            in
            let n_runs =
              let runs_dir = Day11_batch.Snapshot.runs_dir snap_dir in
              match Bos.OS.Dir.contents runs_dir with
              | Ok dirs -> List.length dirs
              | Error _ -> 0
            in
            let repos =
              match Day11_batch.Snapshot.load snap_dir with
              | Ok s ->
                  String.concat ", "
                    (List.map
                       (fun (_, sha) ->
                         String.sub sha 0 (min 12 (String.length sha)))
                       s.repos)
              | Error _ -> "?"
            in
            Printf.printf "  %s  %s  %d solutions, %d runs  [%s]\n" key created
              n_solutions n_runs repos)
          snaps;
        0)

let cmd =
  let doc = "List snapshots for a profile" in
  let info = Cmd.info "snapshots" ~doc in
  Cmd.v info Term.(const run $ Common.profile_term $ Common.profile_dir_term)
