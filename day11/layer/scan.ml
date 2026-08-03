let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let list_layers env dir =
  let ep = eio_path env dir in
  if Eio.Path.is_directory ep then
    try
      Eio.Path.read_dir ep
      |> List.filter_map (fun name ->
             let child = Fpath.(dir / name) in
             if Eio.Path.is_directory (eio_path env child) then
               Some (name, child)
             else None)
    with _ -> []
  else []

let list_package_symlinks ?(exclude = []) env packages_dir pkg_str =
  let pkg_dir = Fpath.(packages_dir / pkg_str) in
  let ep = eio_path env pkg_dir in
  if Eio.Path.is_directory ep then
    try
      Eio.Path.read_dir ep
      |> List.filter_map (fun name ->
             if List.mem name exclude then None
             else
               let child = eio_path env Fpath.(pkg_dir / name) in
               match Eio.Path.stat ~follow:false child with
               | stat when stat.kind = `Symbolic_link -> (
                   try Some (name, Eio.Path.read_link child) with _ -> None)
               | _ -> None
               | exception _ -> None)
    with _ -> []
  else []
