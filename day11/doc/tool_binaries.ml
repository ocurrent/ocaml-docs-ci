let switch = "default"

let find_binary (tool : Day11_opam_layer.Tool.t) name =
  let path =
    Fpath.(tool.dir / "fs" / "home" / "opam" / ".opam" / switch / "bin" / name)
  in
  if Bos.OS.File.exists path |> Result.get_ok then Some path
  else
    (* Try searching across all build layers *)
    List.find_map
      (fun (b : Day11_opam_layer.Build.t) ->
        let os_dir = Fpath.parent tool.dir in
        let bdir = Day11_opam_layer.Build.dir ~os_dir b in
        let p =
          Fpath.(
            bdir / "fs" / "home" / "opam" / ".opam" / switch / "bin" / name)
        in
        if Bos.OS.File.exists p |> Result.get_ok then Some p else None)
      tool.builds

let doc_tool_mounts tool =
  let container_bin_dir = "/home/opam/doc-tools/bin" in
  let odoc_bin = container_bin_dir ^ "/odoc" in
  let odoc_md_bin = container_bin_dir ^ "/odoc-md" in
  let binaries =
    [
      ("odoc", odoc_bin);
      ("odoc-md", odoc_md_bin);
      ("odoc_driver_voodoo", container_bin_dir ^ "/odoc_driver_voodoo");
      ("sherlodoc", container_bin_dir ^ "/sherlodoc");
    ]
  in
  let mounts =
    List.filter_map
      (fun (name, container_path) ->
        match find_binary tool name with
        | Some host_path ->
            Some
              (Day11_container.Mount.bind_ro
                 ~src:(Fpath.to_string host_path)
                 container_path)
        | None -> None)
      binaries
  in
  (mounts, odoc_bin, odoc_md_bin)
