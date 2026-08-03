let src =
  Logs.Src.create "day11.sys.atomic_swap" ~doc:"Atomic directory replacement"

module Log = (val Logs.src_log src)

let staging_of target = Fpath.(target + ".new")
let old_of target = Fpath.(target + ".old")

let cleanup_stale ~sw env dir =
  let rec walk d =
    if Bos.OS.Dir.exists d |> Result.get_ok then
      let entries = Bos.OS.Dir.contents d |> Result.get_ok in
      List.iter
        (fun entry ->
          let name = Fpath.basename entry in
          let is_stale =
            Astring.String.is_suffix ~affix:".new" name
            || Astring.String.is_suffix ~affix:".old" name
          in
          if is_stale then (
            Log.info (fun m -> m "Removing stale dir: %a" Fpath.pp entry);
            Sudo.rm_rf ~sw env entry |> ignore)
          else if Bos.OS.Dir.exists entry |> Result.get_ok then walk entry)
        entries
  in
  walk dir;
  Ok ()

let prepare target =
  let staging = staging_of target in
  (* Remove any existing staging dir from a failed attempt *)
  if Bos.OS.Dir.exists staging |> Result.get_ok then
    Bos.OS.Path.delete ~recurse:true staging |> ignore;
  Bos.OS.Dir.create ~path:true staging
  |> Result.map (fun _created -> staging)
  |> Result.map_error (fun (`Msg m) -> `Msg m)

let commit ~sw env target =
  let staging = staging_of target in
  let old = old_of target in
  if not (Bos.OS.Dir.exists staging |> Result.get_ok) then Ok false
  else
    let has_existing = Bos.OS.Dir.exists target |> Result.get_ok in
    try
      if has_existing then (
        (* Remove any leftover .old *)
        if Bos.OS.Dir.exists old |> Result.get_ok then
          Sudo.rm_rf ~sw env old |> ignore;
        (* Move existing to .old *)
        Unix.rename (Fpath.to_string target) (Fpath.to_string old));
      (* Move staging to final *)
      Unix.rename (Fpath.to_string staging) (Fpath.to_string target);
      (* Remove .old backup *)
      if has_existing && Bos.OS.Dir.exists old |> Result.get_ok then
        Sudo.rm_rf ~sw env old |> ignore;
      Ok true
    with Unix.Unix_error (e, fn, arg) ->
      (* Try to restore *)
      (if
         has_existing
         && Bos.OS.Dir.exists old |> Result.get_ok
         && not (Bos.OS.Dir.exists target |> Result.get_ok)
       then
         try Unix.rename (Fpath.to_string old) (Fpath.to_string target)
         with _ -> ());
      Rresult.R.error_msgf "atomic_swap commit: %s(%s): %s" fn arg
        (Unix.error_message e)

let rollback ~sw env target =
  let staging = staging_of target in
  if Bos.OS.Dir.exists staging |> Result.get_ok then Sudo.rm_rf ~sw env staging
  else Ok ()
