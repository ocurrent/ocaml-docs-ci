let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let ensure env ~packages_dir ~id ~layer_name =
  let id_dir = Fpath.(packages_dir / id) in
  let symlink_path = Fpath.(id_dir / layer_name) in
  let target = Filename.concat ".." (Filename.concat ".." layer_name) in
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 (eio_path env id_dir);
    (* Remove existing entry and recreate — ensures it points to
       the latest build, not just the first one ever created *)
    let ep_link = eio_path env symlink_path in
    (try Eio.Path.unlink ep_link with _ -> ());
    Eio.Path.symlink ~link_to:target ep_link;
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Symlinks.ensure %s/%s: %s" id layer_name
      (Printexc.to_string exn)
