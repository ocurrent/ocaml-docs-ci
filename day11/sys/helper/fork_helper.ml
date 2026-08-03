(* Minimal fork helper daemon.

   Listens on a Unix domain socket. Each connection is a spawn request:
   the client sends argv + env + output mode, the helper forks (cheap —
   this process is a few MB) and execs the command, then sends back the
   exit status and captured output.

   Protocol (binary, big-endian):

   Request:
     u32  env_count
     for each: u32 len, bytes
     u32  argv_count
     for each: u32 len, bytes
     u8   mode: 0 = pipe (capture stdout+stderr), 1 = file
     if file: u32 len, bytes (path)

   Response:
     u8   0 = exited, 1 = signaled
     i32  status code
     u32  stdout_len, stdout_bytes
     u32  stderr_len, stderr_bytes *)

let read_exactly fd buf off len =
  let rec loop off remaining =
    if remaining > 0 then (
      let n = Unix.read fd buf off remaining in
      if n = 0 then raise End_of_file;
      loop (off + n) (remaining - n))
  in
  loop off len

let read_u32 fd =
  let buf = Bytes.create 4 in
  read_exactly fd buf 0 4;
  Bytes.get_int32_be buf 0 |> Int32.to_int

let read_u8 fd =
  let buf = Bytes.create 1 in
  read_exactly fd buf 0 1;
  Bytes.get_uint8 buf 0

let read_str fd =
  let len = read_u32 fd in
  let buf = Bytes.create len in
  read_exactly fd buf 0 len;
  Bytes.to_string buf

let write_all fd buf off len =
  let rec loop off remaining =
    if remaining > 0 then
      let n = Unix.write fd buf off remaining in
      loop (off + n) (remaining - n)
  in
  loop off len

let write_u32 fd n =
  let buf = Bytes.create 4 in
  Bytes.set_int32_be buf 0 (Int32.of_int n);
  write_all fd buf 0 4

let write_u8 fd n =
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 n;
  write_all fd buf 0 1

let write_str fd s =
  write_u32 fd (String.length s);
  write_all fd (Bytes.of_string s) 0 (String.length s)

let read_request fd =
  let n_env = read_u32 fd in
  let env = Array.init n_env (fun _ -> read_str fd) in
  let n_argv = read_u32 fd in
  let argv = Array.init n_argv (fun _ -> read_str fd) in
  let mode = read_u8 fd in
  let output_file = if mode = 1 then Some (read_str fd) else None in
  (env, argv, output_file)

let write_response fd status stdout stderr =
  (match status with
  | Unix.WEXITED n ->
      write_u8 fd 0;
      write_u32 fd n
  | Unix.WSIGNALED n ->
      write_u8 fd 1;
      write_u32 fd n
  | Unix.WSTOPPED n ->
      write_u8 fd 1;
      write_u32 fd n);
  write_str fd stdout;
  write_str fd stderr

(* Read from two FDs concurrently using select, returns (stdout, stderr).
   [on_idle] fires on each select timeout — used by the handler child
   to check its ppid, SIGKILL the worker, and exit if the top-level
   parent has died. Otherwise pipe mode would block indefinitely
   waiting for the worker's fds to close. *)
let read_pipes ?(on_idle = fun () -> ()) r_out r_err =
  let stdout_buf = Buffer.create 4096 in
  let stderr_buf = Buffer.create 4096 in
  let chunk = Bytes.create 65536 in
  let fds = ref [ r_out; r_err ] in
  while !fds <> [] do
    let readable =
      try
        let r, _, _ = Unix.select !fds [] [] 1.0 in
        r
      with Unix.Unix_error (Unix.EINTR, _, _) -> []
    in
    match readable with
    | [] -> on_idle ()
    | _ ->
        List.iter
          (fun fd ->
            let n = Unix.read fd chunk 0 65536 in
            if n = 0 then fds := List.filter (fun f -> f <> fd) !fds
            else if fd = r_out then Buffer.add_subbytes stdout_buf chunk 0 n
            else Buffer.add_subbytes stderr_buf chunk 0 n)
          readable
  done;
  Unix.close r_out;
  Unix.close r_err;
  (Buffer.contents stdout_buf, Buffer.contents stderr_buf)

(* Wait for [worker_pid] with a parent-death watchdog: poll
   [waitpid WNOHANG]; if our original parent dies (ppid changes from
   [orig_ppid]), SIGKILL the worker and exit — so a [kill -9] on the
   top-level process cascades down and doesn't leave solver/build
   processes burning CPU.

   The poll interval starts tiny (5ms) and backs off, capped at 0.5s.
   The vast majority of privileged commands (overlay mount/umount,
   chown, rm) finish in milliseconds, so the old fixed 1.0s sleep
   added ~1s of pure latency to *every* one — and a layer build runs
   6-8 of them, so ~6-8s of dead time per build regardless of package
   size. Fine-grained early polling catches those immediately; the
   back-off keeps long-running commands (the build container itself,
   which can run for minutes) from busy-spinning while still bounding
   parent-death detection to 0.5s. *)
let wait_with_watchdog ~orig_ppid worker_pid =
  let rec loop delay =
    match Unix.waitpid [ WNOHANG ] worker_pid with
    | 0, _ ->
        if Unix.getppid () <> orig_ppid then (
          (try Unix.kill worker_pid Sys.sigkill with _ -> ());
          (try ignore (Unix.waitpid [] worker_pid) with _ -> ());
          exit 0);
        (try Unix.sleepf delay with Unix.Unix_error (Unix.EINTR, _, _) -> ());
        loop (Float.min 0.5 (delay *. 2.))
    | _pid, status -> status
    | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Unix.WEXITED 127
  in
  loop 0.005

let handle_connection fd =
  let orig_ppid = Unix.getppid () in
  let env, argv, output_file = read_request fd in
  let status, stdout, stderr =
    try
      match output_file with
      | Some path ->
          let out_fd =
            Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644
          in
          let pid =
            Unix.create_process_env argv.(0) argv env Unix.stdin out_fd out_fd
          in
          Unix.close out_fd;
          let status = wait_with_watchdog ~orig_ppid pid in
          let output =
            try In_channel.with_open_text path In_channel.input_all
            with _ -> ""
          in
          (status, output, "")
      | None ->
          let r_out, w_out = Unix.pipe () in
          let r_err, w_err = Unix.pipe () in
          let pid =
            Unix.create_process_env argv.(0) argv env Unix.stdin w_out w_err
          in
          Unix.close w_out;
          Unix.close w_err;
          let on_idle () =
            if Unix.getppid () <> orig_ppid then (
              (try Unix.kill pid Sys.sigkill with _ -> ());
              (try ignore (Unix.waitpid [] pid) with _ -> ());
              exit 0)
          in
          let stdout, stderr = read_pipes ~on_idle r_out r_err in
          let status = wait_with_watchdog ~orig_ppid pid in
          (status, stdout, stderr)
    with exn -> (Unix.WEXITED 127, "", Printexc.to_string exn)
  in
  write_response fd status stdout stderr;
  Unix.close fd

let reap_handlers () =
  let rec loop () =
    match Unix.waitpid [ WNOHANG ] (-1) with
    | 0, _ | (exception Unix.Unix_error (ECHILD, _, _)) -> ()
    | _ -> loop ()
  in
  loop ()

let () =
  let sock_path = Sys.argv.(1) in
  (* Snapshot parent pid at startup so the watchdog can detect a
     parent-death by comparison. If [kill -9] hits the parent, our
     ppid flips to 1 (or whatever init is in this pid namespace),
     differing from the snapshot — time to exit. *)
  let orig_ppid = Unix.getppid () in
  (* Install SIGCHLD handler to reap handler children promptly *)
  Sys.set_signal Sys.sigchld (Sys.Signal_handle (fun _ -> reap_handlers ()));
  (* Polite shutdown paths: [Fork_client.stop] sends SIGTERM, shell
     sends SIGHUP. Both should flush [at_exit] and quit cleanly. *)
  let exit_on_signal _ = exit 0 in
  Sys.set_signal Sys.sigterm (Sys.Signal_handle exit_on_signal);
  Sys.set_signal Sys.sighup (Sys.Signal_handle exit_on_signal);
  let sock = Unix.socket PF_UNIX SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_UNIX sock_path);
  Unix.listen sock 64;
  while true do
    if Unix.getppid () <> orig_ppid then exit 0;
    let readable =
      try
        let r, _, _ = Unix.select [ sock ] [] [] 1.0 in
        r
      with Unix.Unix_error (EINTR, _, _) -> []
    in
    match readable with
    | [] -> () (* timeout — loop back to the watchdog check *)
    | _ -> (
        match Unix.accept sock with
        | exception Unix.Unix_error (EINTR, _, _) -> ()
        | fd, _ ->
            let pid = Unix.fork () in
            if pid = 0 then (
              (* Handler child *)
              Unix.close sock;
              (* Reset SIGCHLD so waitpid works for command children *)
              Sys.set_signal Sys.sigchld Sys.Signal_default;
              (try handle_connection fd
               with e ->
                 Printf.eprintf "fork_helper: %s\n%!" (Printexc.to_string e));
              exit 0)
            else Unix.close fd)
  done
