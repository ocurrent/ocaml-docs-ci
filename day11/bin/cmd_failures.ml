(** failures command: list failing packages *)

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
          let packages_dir = Fpath.(snapshot_dir / "packages") in
          let pkg_dir_s = Fpath.to_string packages_dir in
          let from_history () =
            let pkgs =
              Sys.readdir pkg_dir_s |> Array.to_list |> List.sort compare
            in
            List.filter_map
              (fun pkg_str ->
                let entries = Day11_lib.History.read ~packages_dir ~pkg_str in
                match entries with
                | [] -> None
                | latest :: _ ->
                    if latest.status = "failure" then
                      Some
                        ( pkg_str,
                          latest.category,
                          Option.value ~default:"" latest.error )
                    else None)
              pkgs
          in
          let from_live_view () =
            match Day11_batch.Live_view.load_latest ~snapshot_dir with
            | None -> []
            | Some lv ->
                Hashtbl.fold
                  (fun pkg status acc ->
                    match status with
                    | "fail" -> (pkg, "build_failure", "") :: acc
                    | "cascade" ->
                        let dep =
                          try Hashtbl.find lv.failed_dep pkg
                          with Not_found -> ""
                        in
                        let note =
                          if dep = "" then "cascade"
                          else "cascade (blocked by " ^ dep ^ ")"
                        in
                        (pkg, note, "") :: acc
                    | _ -> acc)
                  lv.pkg_status []
          in
          let live = not (Day11_batch.Live_view.is_terminal ~snapshot_dir) in
          let failures =
            if Sys.file_exists pkg_dir_s && not live then from_history ()
            else from_live_view ()
          in
          (if live then
             match Day11_batch.Live_view.load_latest ~snapshot_dir with
             | Some lv ->
                 Printf.printf "Run %s (in progress) — %s\n\n" lv.run_id
                   (Day11_batch.Live_view.format_progress lv.progress)
             | None -> ());
          (if failures = [] then Printf.printf "No failures\n"
           else
             let failures = List.sort compare failures in
             Printf.printf "%d failures:\n\n" (List.length failures);
             List.iter
               (fun (pkg, cat, err) ->
                 Printf.printf "  %-40s  %s%s\n" pkg cat
                   (if err = "" then "" else "  " ^ err))
               failures);
          0)

let cmd =
  let info = Cmd.info "failures" ~doc:"List packages with failing builds" in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
