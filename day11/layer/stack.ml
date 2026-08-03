let src_log = Logs.Src.create "day11.layer.stack" ~doc:"Layer stacking"

module Log = (val Logs.src_log src_log)

let plan_lowerdir ~available ~merged_overhead ~entry_cost layer_dirs =
  let total_cost =
    List.fold_left (fun acc d -> acc + entry_cost d) 0 layer_dirs
  in
  (* Fast path: every layer fits as its own lowerdir, no merge needed. *)
  if total_cost <= available then (layer_dirs, [])
  else
    (* Walk the list taking layers for the separate bucket as long
       as we stay within (available - merged_overhead). Once we'd
       overflow, the rest of the list goes to the merged bucket. *)
    let target = available - merged_overhead in
    let rec aux acc_cost kept = function
      | [] -> (List.rev kept, [])
      | d :: rest ->
          let c = entry_cost d in
          if acc_cost + c <= target then aux (acc_cost + c) (d :: kept) rest
          else (List.rev kept, d :: rest)
    in
    aux 0 [] layer_dirs

let collect_fs_dirs env layer_dirs =
  List.filter_map
    (fun layer_dir ->
      let fs = Fpath.(layer_dir / "fs") in
      if Eio.Path.is_directory Eio.Path.(env#fs / Fpath.to_string fs) then
        Some (Fpath.to_string fs)
      else (
        Log.debug (fun m -> m "Skipping %a (no fs/ subdir)" Fpath.pp layer_dir);
        None))
    layer_dirs

let merge_script fs_dirs target =
  let target_s = Fpath.to_string target in
  let cmds =
    List.map
      (fun fs ->
        Printf.sprintf
          "cp -n --archive --no-dereference --recursive --link \
           --no-target-directory %s %s"
          (Filename.quote fs) (Filename.quote target_s))
      fs_dirs
  in
  "r=0; "
  ^ String.concat " " (List.map (fun c -> c ^ " || r=1;") cmds)
  ^ " exit $r"

let merge ~sw env ~layer_dirs ~target =
  let fs_dirs = collect_fs_dirs env layer_dirs in
  if fs_dirs = [] then Ok ()
  else (
    Log.info (fun m ->
        m "Merging %d layer fs dirs into %a (sudo)" (List.length fs_dirs)
          Fpath.pp target);
    let script = merge_script fs_dirs target in
    let result =
      Day11_sys.Sudo.run ~sw env Bos.Cmd.(v "bash" % "-c" % script)
    in
    match result with Ok _ -> Ok () | Error _ as e -> e)

let merge_no_sudo env ~layer_dirs ~target =
  let fs_dirs = collect_fs_dirs env layer_dirs in
  if fs_dirs = [] then Ok ()
  else (
    Log.info (fun m ->
        m "Merging %d layer fs dirs into %a" (List.length fs_dirs) Fpath.pp
          target);
    let script = merge_script fs_dirs target in
    let result = Bos.OS.Cmd.run_status Bos.Cmd.(v "bash" % "-c" % script) in
    match result with
    | Ok (`Exited 0) -> Ok ()
    | Ok (`Exited n) ->
        Rresult.R.error_msgf "cp --link into %a exited %d" Fpath.pp target n
    | Ok (`Signaled n) ->
        Rresult.R.error_msgf "cp --link into %a signaled %d" Fpath.pp target n
    | Error _ as e -> e)
