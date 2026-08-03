(** log command: view build or doc log *)

open Cmdliner
module Layer = Day11_layer.Layer

let resolve_layer env os_dir ~packages_dir arg =
  (* If it looks like a layer dir name and exists, use it *)
  let layer_dir = Fpath.(os_dir / arg) in
  if Bos.OS.Dir.exists layer_dir |> Result.get_ok then Some arg
  else
    (* Try to look up as a package name in the packages dir *)
    let symlinks =
      Day11_layer.Scan.list_package_symlinks
        ~exclude:[ "blessed-build"; "blessed-docs"; "history.jsonl" ]
        env packages_dir arg
    in
    match symlinks with
    | [] -> None
    | links -> (
        (* Pick the most recent layer (last alphabetically, since build-<hash>
         names are stable but we want the latest symlink by mtime) *)
        let ranked =
          List.filter_map
            (fun (name, _target) ->
              let path = Fpath.(packages_dir / arg / name) in
              try
                let stat = Unix.stat (Fpath.to_string path) in
                Some (name, stat.Unix.st_mtime)
              with Unix.Unix_error _ -> None)
            links
        in
        let sorted = List.sort (fun (_, t1) (_, t2) -> compare t2 t1) ranked in
        match sorted with (name, _) :: _ -> Some name | [] -> None)

let run profile_name profile_dir arg =
  Common.with_eio @@ fun ~sw:_ env ->
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) -> (
      let os_dir = paths.os_dir in
      let packages_dir =
        match Common.latest_snapshot_dir paths with
        | Some sd -> Fpath.(sd / "packages")
        | None -> Fpath.(os_dir / "packages")
      in
      let layer =
        match resolve_layer env os_dir ~packages_dir arg with
        | Some l -> l
        | None ->
            Printf.eprintf "No layer or package found for %s\n" arg;
            exit 1
      in
      let ly = Layer.of_hash ~os_dir layer in
      let layer_dir = Layer.dir ly in
      (* Try layer.log first (build), then odoc-voodoo-all.log (doc) *)
      let log_file =
        let build_log = Layer.log_path ly in
        let doc_log = Fpath.(layer_dir / "odoc-voodoo-all.log") in
        if Bos.OS.File.exists build_log |> Result.get_ok then Some build_log
        else if Bos.OS.File.exists doc_log |> Result.get_ok then Some doc_log
        else None
      in
      match log_file with
      | None ->
          Printf.eprintf "No log found in %s\n" layer;
          1
      | Some path -> (
          match Bos.OS.File.read path with
          | Ok content ->
              print_string content;
              0
          | Error (`Msg e) ->
              Printf.eprintf "%s\n" e;
              1))

let target_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"TARGET"
        ~doc:
          "Layer directory name (e.g. build-abc123) or package name (e.g. \
           astring.0.8.5)")

let cmd =
  let info = Cmd.info "log" ~doc:"View build or doc log" in
  let term =
    Term.(
      const run $ Common.profile_term $ Common.profile_dir_term $ target_term)
  in
  Cmd.v info term
