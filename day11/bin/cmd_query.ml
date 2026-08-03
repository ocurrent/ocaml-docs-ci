(** query command: show detailed info for a package *)

open Cmdliner
module Layer = Day11_layer.Layer

let show_layers_from_symlinks env ~os_dir ~packages_dir ~pkg_str =
  let symlinks =
    Day11_layer.Scan.list_package_symlinks
      ~exclude:[ "blessed-build"; "blessed-docs"; "history.jsonl" ]
      env packages_dir pkg_str
  in
  if symlinks = [] then (
    Printf.printf "No layers for %s\n" pkg_str;
    1)
  else (
    Printf.printf "Layers for %s (%d):\n\n" pkg_str (List.length symlinks);
    List.iter
      (fun (name, _target) ->
        let layer = Layer.of_hash ~os_dir name in
        match Day11_layer.Meta.load env (Layer.meta_path layer) with
        | Ok meta ->
            let status =
              if meta.exit_status = 0 then "ok"
              else if meta.failed_dep <> None then "cascade"
              else "fail"
            in
            Printf.printf "  %s  status=%s  created=%s\n" name status
              meta.created_at
        | Error _ -> Printf.printf "  %s  (no metadata)\n" name)
      symlinks;
    0)

let run profile_name profile_dir package =
  Common.with_eio @@ fun ~sw:_ env ->
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
          let os_dir = paths.os_dir in
          let packages_dir = Fpath.(snapshot_dir / "packages") in
          let entries = Day11_lib.History.read ~packages_dir ~pkg_str:package in
          if entries = [] then
            (* Fall back to showing layers discovered via symlinks *)
            show_layers_from_symlinks env ~os_dir ~packages_dir ~pkg_str:package
          else (
            Printf.printf "History for %s (%d entries):\n\n" package
              (List.length entries);
            List.iter
              (fun (e : Day11_lib.History.entry) ->
                Printf.printf
                  "  %s  run=%s  hash=%s  status=%s  category=%s%s\n" e.ts e.run
                  (String.sub e.build_hash 0
                     (min 16 (String.length e.build_hash)))
                  e.status e.category
                  (if e.blessed then " [blessed]" else ""))
              entries;
            0))

let package_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PACKAGE" ~doc:"Package name (e.g. astring.0.8.5)")

let cmd =
  let info = Cmd.info "query" ~doc:"Show build history for a package" in
  let term =
    Term.(
      const run $ Common.profile_term $ Common.profile_dir_term $ package_term)
  in
  Cmd.v info term
