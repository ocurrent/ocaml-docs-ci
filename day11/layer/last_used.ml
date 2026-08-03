let sentinel = "last_used"
let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let touch env layer_dir =
  let path = Fpath.(layer_dir / sentinel) in
  let ep = eio_path env path in
  try
    (* Create the file if it doesn't exist. *)
    Eio.Path.with_open_out ~create:(`If_missing 0o644) ep (fun _ -> ());
    (* Update mtime to now via a systhread; Eio.Path has no utimes. *)
    let path_s = Fpath.to_string path in
    Eio_unix.run_in_systhread (fun () -> Unix.utimes path_s 0.0 0.0)
  with _ -> ()

let get env layer_dir =
  let ep = eio_path env Fpath.(layer_dir / sentinel) in
  try Some (Eio.Path.stat ~follow:true ep).mtime with _ -> None

let effective env layer_dir =
  match get env layer_dir with
  | Some _ as t -> t
  | None -> (
      (* No sentinel: fall back to when the layer itself was written.
       [layer.json] is created at finalisation and only rewritten by
       the same build, so its mtime is the layer's own age. *)
      let ep = eio_path env Fpath.(layer_dir / "layer.json") in
      try Some (Eio.Path.stat ~follow:true ep).mtime with _ -> None)
