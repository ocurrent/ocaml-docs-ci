let nproc () =
  let ic = Unix.open_process_in "nproc" in
  let n =
    In_channel.input_line ic |> Option.get |> String.trim |> int_of_string
  in
  ignore (Unix.close_process_in ic);
  n

let dir_size path =
  let rec walk acc dir =
    let entries = Bos.OS.Dir.contents dir |> Result.get_ok in
    List.fold_left
      (fun acc entry ->
        let stat = Unix.lstat (Fpath.to_string entry) in
        match stat.Unix.st_kind with
        | Unix.S_DIR -> walk acc entry
        | Unix.S_REG -> acc + stat.Unix.st_size
        | _ -> acc)
      acc entries
  in
  try Ok (walk 0 path) with
  | Unix.Unix_error (e, fn, arg) ->
      Rresult.R.error_msgf "%s(%s): %s" fn arg (Unix.error_message e)
  | exn -> Rresult.R.error_msgf "dir_size: %s" (Printexc.to_string exn)
