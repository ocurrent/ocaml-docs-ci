let src = Logs.Src.create "day11.sys.dir_lock" ~doc:"Directory-level locking"

module Log = (val Logs.src_log src)

(* Intra-process mutex table. Since Eio is cooperative, hashtable access
   between yield points is safe without additional synchronization. *)
let mutex_table : (string, Eio.Mutex.t) Hashtbl.t = Hashtbl.create 64

let get_mutex key =
  match Hashtbl.find_opt mutex_table key with
  | Some m -> m
  | None ->
      let m = Eio.Mutex.create () in
      Hashtbl.replace mutex_table key m;
      m

let default_lock_file dir_path = Fpath.(dir_path + ".lock")

let with_lock ?marker_file ?lock_file dir_path body =
  (* Check marker file first — if it exists, skip entirely *)
  (match marker_file with
    | Some marker ->
        let marker_path = Fpath.(dir_path // marker) in
        if Bos.OS.File.exists marker_path |> Result.get_ok then (
          Log.debug (fun m ->
              m "Marker %a exists, skipping" Fpath.pp marker_path);
          Ok () |> fun r -> r)
        else Error `Continue
    | None -> Error `Continue)
  |> function
  | Ok () -> Ok ()
  | Error `Continue ->
      let lock_file =
        match lock_file with Some f -> f | None -> default_lock_file dir_path
      in
      let lock_file_s = Fpath.to_string lock_file in
      (* Acquire intra-process Eio mutex *)
      let eio_mutex = get_mutex lock_file_s in
      Eio.Mutex.lock eio_mutex;
      Fun.protect
        ~finally:(fun () -> Eio.Mutex.unlock eio_mutex)
        (fun () ->
          (* Re-check marker after acquiring mutex — another fiber may have
       completed while we waited *)
          (match marker_file with
            | Some marker ->
                let marker_path = Fpath.(dir_path // marker) in
                if Bos.OS.File.exists marker_path |> Result.get_ok then (
                  Log.debug (fun m ->
                      m "Marker %a appeared while waiting for lock" Fpath.pp
                        marker_path);
                  Ok () |> fun r -> r)
                else Error `Continue
            | None -> Error `Continue)
          |> function
          | Ok () -> Ok ()
          | Error `Continue ->
              (* Ensure parent directory of lock file exists *)
              Bos.OS.Dir.create ~path:true (Fpath.parent lock_file) |> ignore;
              (* Acquire cross-process file lock *)
              let lock_fd =
                Unix.openfile lock_file_s [ Unix.O_CREAT; Unix.O_RDWR ] 0o644
              in
              Fun.protect
                ~finally:(fun () -> Unix.close lock_fd)
                (fun () ->
                  let got_immediately =
                    try
                      Unix.lockf lock_fd Unix.F_TLOCK 0;
                      true
                    with
                    | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
                      false
                  in
                  if not got_immediately then (
                    Log.info (fun m ->
                        m "Waiting for lock: %a" Fpath.pp dir_path);
                    let rec lock_retry () =
                      try Unix.lockf lock_fd Unix.F_LOCK 0
                      with Unix.Unix_error (Unix.EINTR, _, _) -> lock_retry ()
                    in
                    lock_retry ();
                    Log.info (fun m -> m "Acquired lock: %a" Fpath.pp dir_path));
                  (* Write metadata: PID and timestamp *)
                  let set_temp_log_path log_path =
                    let metadata =
                      Printf.sprintf "%d\n%.0f\n%s\n" (Unix.getpid ())
                        (Unix.time ()) (Fpath.to_string log_path)
                    in
                    ignore (Unix.lseek lock_fd 0 Unix.SEEK_SET);
                    ignore (Unix.ftruncate lock_fd 0);
                    let bytes = Bytes.of_string metadata in
                    ignore (Unix.write lock_fd bytes 0 (Bytes.length bytes))
                  in
                  (* Re-check marker after file lock *)
                  match marker_file with
                  | Some marker ->
                      let marker_path = Fpath.(dir_path // marker) in
                      if Bos.OS.File.exists marker_path |> Result.get_ok then (
                        Log.debug (fun m ->
                            m "Marker %a appeared while waiting for file lock"
                              Fpath.pp marker_path);
                        Ok ())
                      else body ~set_temp_log_path dir_path
                  | None -> body ~set_temp_log_path dir_path))
