type doc_layer = {
  pkg : OpamPackage.t;
  layer_hash : string;
  prep_path : Fpath.t;
  universe : string;
  blessed : bool;
}

let is_doc_layer name =
  Astring.String.is_prefix ~affix:"doc-" name
  && (not (Astring.String.is_prefix ~affix:"doc-driver-" name))
  && not (Astring.String.is_prefix ~affix:"doc-odoc-" name)

let scan_cache env ~os_dir =
  let layers = Day11_layer.Scan.list_layers env os_dir in
  List.filter_map
    (fun (name, layer_path) ->
      if not (is_doc_layer name) then None
      else
        let json_path = Fpath.(layer_path / "layer.json") in
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
                let _html_path = doc |> member "html_path" |> to_string in
                let blessed = doc |> member "blessed" |> to_bool in
                let dep_hashes =
                  json
                  |> member "dep_doc_hashes"
                  |> to_list
                  |> List.map to_string
                in
                let universe = Command.compute_universe_hash dep_hashes in
                Some
                  {
                    pkg;
                    layer_hash = name;
                    prep_path = Fpath.(layer_path / "prep");
                    universe;
                    blessed;
                  }
            with _ -> None))
    layers

let mount_overlay ~sw env ~layers ~mount_point ~work_dir =
  if layers = [] then Rresult.R.error_msgf "Combine: no layers to mount"
  else
    let lower_dirs = List.map (fun l -> l.prep_path) layers in
    let upper = Fpath.(work_dir / "upper") in
    let work = Fpath.(work_dir / "work") in
    Bos.OS.Dir.create ~path:true upper |> ignore;
    Bos.OS.Dir.create ~path:true work |> ignore;
    Day11_container.Overlay.mount ~sw env ~lower:lower_dirs ~upper ~work
      ~target:mount_point

let unmount ~sw env mount_point =
  Day11_container.Overlay.umount ~sw env mount_point
