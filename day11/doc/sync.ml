type doc_entry = {
  pkg : OpamPackage.t;
  html_path : Fpath.t;
  universe : string;
  blessed : bool;
}

let scan_cache env ~os_dir =
  let layers = Day11_layer.Scan.list_layers env os_dir in
  List.filter_map
    (fun (name, path) ->
      if
        (not (Astring.String.is_prefix ~affix:"doc-" name))
        || Astring.String.is_prefix ~affix:"doc-driver-" name
        || Astring.String.is_prefix ~affix:"doc-odoc-" name
      then None
      else
        let json_path = Fpath.(path / "layer.json") in
        match
          try Some (Yojson.Safe.from_file (Fpath.to_string json_path))
          with _ -> None
        with
        | None -> None
        | Some json -> (
            try
              let open Yojson.Safe.Util in
              let pkg_str = json |> member "package" |> to_string in
              let pkg = OpamPackage.of_string pkg_str in
              let doc = json |> member "doc" in
              let status = doc |> member "status" |> to_string in
              if status <> "success" then None
              else
                let html_path = doc |> member "html_path" |> to_string in
                let blessed = doc |> member "blessed" |> to_bool in
                let dep_hashes =
                  json
                  |> member "dep_doc_hashes"
                  |> to_list
                  |> List.map to_string
                in
                let universe = Command.compute_universe_hash dep_hashes in
                Some { pkg; html_path = Fpath.v html_path; universe; blessed }
            with _ -> None))
    layers

let sync ~sw env ~entries ~destination ?(blessed_only = false)
    ?(package_filter = fun _ -> true) ?(dry_run = false) ?on_progress () =
  let filtered =
    entries
    |> List.filter (fun e ->
        ((not blessed_only) || e.blessed)
        && package_filter (OpamPackage.to_string e.pkg))
  in
  let total = List.length filtered in
  let done_count = ref 0 in
  let results =
    List.map
      (fun entry ->
        let src = Fpath.to_string entry.html_path ^ "/" in
        let dst_subpath =
          if entry.blessed then
            Printf.sprintf "%s/%s/"
              (OpamPackage.name_to_string entry.pkg)
              (OpamPackage.version_to_string entry.pkg)
          else
            Printf.sprintf "universes/%s/%s/%s/" entry.universe
              (OpamPackage.name_to_string entry.pkg)
              (OpamPackage.version_to_string entry.pkg)
        in
        let dst = destination ^ "/" ^ dst_subpath in
        let cmd =
          Bos.Cmd.(
            v "rsync"
            % "-av"
            %% (if dry_run then Bos.Cmd.v "--dry-run" else Bos.Cmd.empty)
            % "--delete"
            % src
            % dst)
        in
        let result =
          match Day11_sys.Run.run ~sw env cmd None with
          | run when run.Day11_sys.Run.status = `Exited 0 -> Ok ()
          | run ->
              Rresult.R.error_msgf "rsync failed for %s: %s"
                (OpamPackage.to_string entry.pkg)
                run.errors
          | exception exn ->
              Rresult.R.error_msgf "rsync: %s" (Printexc.to_string exn)
        in
        incr done_count;
        (match on_progress with
        | Some f -> f ~done_count:!done_count ~total ~pkg:entry.pkg
        | None -> ());
        result)
      filtered
  in
  match List.find_opt Result.is_error results with
  | Some (Error _ as e) -> e
  | _ -> Ok ()
