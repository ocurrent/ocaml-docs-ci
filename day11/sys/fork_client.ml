(* Client for the fork helper daemon.

   Sends spawn requests over a Unix socket, avoiding fork from the
   large main process (which costs ~100ms per clone3 due to page table
   copying). The helper is a few MB, so its forks are sub-millisecond. *)

let src = Logs.Src.create "day11.sys.fork_client" ~doc:"Fork helper client"

module Log = (val Logs.src_log src)

type t = { sock_path : string; pid : int }

(* ── Protocol I/O on Eio buffered flows ─────────────────────────── *)

let write_u32 w n = Eio.Buf_write.BE.uint32 w (Int32.of_int n)
let write_u8 w n = Eio.Buf_write.uint8 w n

let write_str w s =
  write_u32 w (String.length s);
  Eio.Buf_write.string w s

let read_u32 r = Eio.Buf_read.BE.uint32 r |> Int32.to_int
let read_u8 r = Eio.Buf_read.uint8 r

let read_str r =
  let len = read_u32 r in
  Eio.Buf_read.take len r

(* ── Helper daemon lifecycle ────────────────────────────────────── *)

let start () =
  let sock_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "day11-fork-%d.sock" (Unix.getpid ()))
  in
  let helper_bin =
    (* [DAY11_FORK_HELPER] overrides the lookup — used by the test suite
       (where the helper isn't on PATH or beside the test binary) and
       available as a deployment hook for non-standard install layouts.
       Otherwise look next to the main binary, then fall back to PATH. *)
    match Sys.getenv_opt "DAY11_FORK_HELPER" with
    | Some p -> p
    | None ->
        let dir = Filename.dirname Sys.executable_name in
        let candidate = Filename.concat dir "day11-fork-helper" in
        if Sys.file_exists candidate then candidate else "day11-fork-helper"
  in
  (* [sock_path] is derived from our pid, which the OS reuses across
     runs. With a persistent TMPDIR (a real fs, as the daemon now
     requires) a stale socket from a prior killed run can survive; the
     helper's [bind] would then fail on the existing file, leaving
     nothing listening and the client seeing [Connection refused].
     Remove any stale socket first so the helper always binds fresh.
     Safe: only one live process can hold our pid, so any existing file
     here belongs to a dead one. *)
  (try Unix.unlink sock_path with _ -> ());
  Log.info (fun m -> m "Starting fork helper: %s %s" helper_bin sock_path);
  let pid =
    Unix.create_process helper_bin
      [| helper_bin; sock_path |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  (* Wait for socket to appear. One-shot startup, before the Eio loop
     does anything interesting — Unix.sleepf is fine here. *)
  let rec wait_sock n =
    if n <= 0 then failwith "fork helper: socket did not appear"
    else if Sys.file_exists sock_path then ()
    else (
      Unix.sleepf 0.01;
      wait_sock (n - 1))
  in
  wait_sock 100;
  Log.info (fun m -> m "Fork helper running (pid %d)" pid);
  { sock_path; pid }

let stop t =
  (try Unix.kill t.pid Sys.sigterm with _ -> ());
  (try ignore (Unix.waitpid [] t.pid) with _ -> ());
  try Unix.unlink t.sock_path with _ -> ()

(* ── Spawn over the Unix socket ─────────────────────────────────── *)

(* Cap on the buffered read of one protocol field. stdout/stderr each
   fit in their own buffer, so a single subprocess output is bounded
   here. Larger captures should use [output_file]. *)
let max_response_field_bytes = 256 * 1024 * 1024

(* Global instance, started lazily.
   Uses Atomic to avoid Mutex deadlocks under Eio — multiple fibers
   on the same OS thread would deadlock an OCaml Mutex.t. *)
let instance : t option Atomic.t = Atomic.make None

let spawn ~sw env t ~env_arr ~argv ~output_file =
  let net = Eio.Stdenv.net env in
  try
    let flow = Eio.Net.connect ~sw net (`Unix t.sock_path) in
    Eio.Buf_write.with_flow flow (fun w ->
        write_u32 w (Array.length env_arr);
        Array.iter (write_str w) env_arr;
        write_u32 w (Array.length argv);
        Array.iter (write_str w) argv;
        match output_file with
        | None -> write_u8 w 0
        | Some path ->
            write_u8 w 1;
            write_str w path);
    let r = Eio.Buf_read.of_flow flow ~max_size:max_response_field_bytes in
    let status_type = read_u8 r in
    let status_code = read_u32 r in
    let stdout = read_str r in
    let stderr = read_str r in
    let status =
      match status_type with
      | 0 -> `Exited status_code
      | _ -> `Signaled status_code
    in
    (stdout, stderr, status)
  with (Eio.Io _ | End_of_file) as exn ->
    (* Connection refused, EOF mid-read, EPIPE — all symptoms of a
       dead helper. Invalidate the cached instance so the next caller
       starts a fresh one. *)
    Log.warn (fun m ->
        m "Helper looks dead (%s); invalidating instance"
          (Printexc.to_string exn));
    (match Atomic.get instance with
    | Some cached when cached == t ->
        (* Only clear if we are still the current instance — another
          fiber may have already replaced us. *)
        ignore (Atomic.compare_and_set instance (Some cached) None)
    | _ -> ());
    raise exn

let get_instance () =
  match Atomic.get instance with
  | Some t -> t
  | None ->
      let t = start () in
      if Atomic.compare_and_set instance None (Some t) then (
        at_exit (fun () -> stop t);
        t)
      else (
        (* Another fiber raced us — discard ours, use theirs *)
        stop t;
        match Atomic.get instance with
        | Some t -> t
        | None -> failwith "fork helper: startup race")
