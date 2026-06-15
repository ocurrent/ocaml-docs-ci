open Bos

type t = {
  cmd : string list;
  time : float;
  output_file : Fpath.t option;
  output : string;
  errors : string;
  status : [ `Exited of int | `Signaled of int ];
}

let src = Logs.Src.create "day11.sys.run" ~doc:"Subprocess execution"
module Log = (val Logs.src_log src)

(* Delegate fork+exec to the fork helper daemon via a Unix socket.
   The helper is a tiny process (~5MB) so its forks are sub-millisecond,
   compared to ~100ms when forking the main 1.7GB process. The socket
   I/O is cooperative on the calling fiber's domain. *)
let run_via_helper ~sw env cmd env_arr output_file =
  let helper = Fork_client.get_instance () in
  let argv = Array.of_list cmd in
  let file_path = Option.map Fpath.to_string output_file in
  Fork_client.spawn ~sw env helper ~env_arr ~argv ~output_file:file_path

let run ~sw env cmd output_file =
  let cmd = Cmd.to_list cmd in
  Log.debug (fun m -> m "Executing: %s" (String.concat " " cmd));
  let t_start = Unix.gettimeofday () in
  let env_arr =
    let cur = OS.Env.current () |> Result.get_ok in
    Astring.String.Map.fold
      (fun k v acc -> (k ^ "=" ^ v) :: acc)
      cur []
    |> Array.of_list
  in
  let clock = Eio.Stdenv.clock env in
  let rec run_with_retry retries =
    try run_via_helper ~sw env cmd env_arr output_file
    with
    | Eio.Buf_read.Buffer_limit_exceeded as exn ->
      (* The subprocess output exceeded the protocol's per-field cap.
         Retrying re-runs the (possibly expensive) process and gets
         the same oversized response — fail immediately instead. The
         caller should redirect bulk output via [output_file]. *)
      Log.err (fun m -> m "Subprocess output exceeded the fork \
        protocol buffer cap (cmd: %s) — not retrying; use an \
        output_file for bulk output" (String.concat " " cmd));
      raise exn
    | exn ->
      if retries > 0 then begin
        Log.warn (fun m -> m "Fork helper failed (%s), retrying (%d left)"
          (Printexc.to_string exn) retries);
        Eio.Time.sleep clock 0.1;
        run_with_retry (retries - 1)
      end else
        raise exn
  in
  let output, errors, status = run_with_retry 3 in
  let t_end = Unix.gettimeofday () in
  let time = t_end -. t_start in
  let result = { cmd; time; output_file; output; errors; status } in
  (match result.status with
  | `Exited 0 -> ()
  | _ ->
      let verb, n =
        match result.status with
        | `Exited n -> ("exited", n)
        | `Signaled n -> ("signaled", n)
      in
      Log.err (fun m ->
          m "Process %s with %d: '%s'\nStdout:\n%s\nStderr:\n%s"
            verb n (String.concat " " result.cmd)
            result.output result.errors));
  result

