let src = Logs.Src.create "day11.sys.tree" ~doc:"Directory tree operations"

module Log = (val Logs.src_log src)

let cp_file ~source ~target =
  let buf_size = 65536 in
  let buf = Bytes.create buf_size in
  let src_s = Fpath.to_string source in
  let tgt_s = Fpath.to_string target in
  let ic = open_in_bin src_s in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let oc = open_out_bin tgt_s in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          let rec loop () =
            let n = input ic buf 0 buf_size in
            if n > 0 then (
              output oc buf 0 n;
              loop ())
          in
          loop ()));
  (* Preserve permissions and times; a failure here shouldn't abort the
     copy that already succeeded. *)
  let stat = Unix.lstat src_s in
  (try Unix.chmod tgt_s stat.Unix.st_perm
   with Unix.Unix_error (e, _, _) ->
     Log.warn (fun m ->
         m "could not preserve permissions on %s: %s" tgt_s
           (Unix.error_message e)));
  try Unix.utimes tgt_s stat.Unix.st_atime stat.Unix.st_mtime
  with Unix.Unix_error (e, _, _) ->
    Log.warn (fun m ->
        m "could not preserve timestamps on %s: %s" tgt_s (Unix.error_message e))

let rec walk_copy ~link source target =
  let src_s = Fpath.to_string source in
  let stat = Unix.lstat src_s in
  match stat.Unix.st_kind with
  | Unix.S_DIR ->
      Bos.OS.Dir.create ~path:true target |> Result.get_ok |> ignore;
      let entries = Bos.OS.Dir.contents source |> Result.get_ok in
      List.iter
        (fun entry ->
          let name = Fpath.basename entry in
          walk_copy ~link entry Fpath.(target / name))
        entries
  | Unix.S_LNK ->
      let link_target = Unix.readlink src_s in
      Unix.symlink link_target (Fpath.to_string target)
  | Unix.S_REG ->
      if link then (
        try Unix.link src_s (Fpath.to_string target)
        with Unix.Unix_error (Unix.EMLINK, _, _) ->
          Log.debug (fun m ->
              m "EMLINK on %a, falling back to copy" Fpath.pp source);
          cp_file ~source ~target)
      else cp_file ~source ~target
  | _ -> ()

let hardlink ~source ~target =
  try
    walk_copy ~link:true source target;
    Ok ()
  with
  | Unix.Unix_error (e, fn, arg) ->
      Rresult.R.error_msgf "hardlink %a -> %a: %s(%s): %s" Fpath.pp source
        Fpath.pp target fn arg (Unix.error_message e)
  | exn -> Rresult.R.error_msgf "hardlink: %s" (Printexc.to_string exn)

let copy ~source ~target =
  try
    walk_copy ~link:false source target;
    Ok ()
  with
  | Unix.Unix_error (e, fn, arg) ->
      Rresult.R.error_msgf "copy %a -> %a: %s(%s): %s" Fpath.pp source Fpath.pp
        target fn arg (Unix.error_message e)
  | exn -> Rresult.R.error_msgf "copy: %s" (Printexc.to_string exn)

let clense ~source ~target =
  let rec walk src tgt =
    let tgt_s = Fpath.to_string tgt in
    let stat = Unix.lstat tgt_s in
    match stat.Unix.st_kind with
    | Unix.S_DIR ->
        let entries = Bos.OS.Dir.contents tgt |> Result.get_ok in
        List.iter
          (fun entry ->
            let name = Fpath.basename entry in
            let src_entry = Fpath.(src / name) in
            if Bos.OS.Path.exists src_entry |> Result.get_ok then
              walk src_entry entry)
          entries;
        (* Remove dir if now empty *)
        let remaining = Bos.OS.Dir.contents tgt |> Result.get_ok in
        if remaining = [] then Bos.OS.Dir.delete ~recurse:false tgt |> ignore
    | Unix.S_REG -> (
        let src_s = Fpath.to_string src in
        try
          let src_stat = Unix.lstat src_s in
          if src_stat.Unix.st_mtime = stat.Unix.st_mtime then (
            try Unix.unlink tgt_s
            with Unix.Unix_error (Unix.EACCES, _, _) ->
              (* Read-only file: add write bits and retry. *)
              Unix.chmod tgt_s (stat.Unix.st_perm lor 0o222);
              Unix.unlink tgt_s)
        with Unix.Unix_error (e, _, _) ->
          Log.debug (fun m ->
              m "clense: skipping %s: %s" tgt_s (Unix.error_message e)))
    | _ -> ()
  in
  try
    walk source target;
    Ok ()
  with
  | Unix.Unix_error (e, fn, arg) ->
      Rresult.R.error_msgf "clense %a -> %a: %s(%s): %s" Fpath.pp source
        Fpath.pp target fn arg (Unix.error_message e)
  | exn -> Rresult.R.error_msgf "clense: %s" (Printexc.to_string exn)
