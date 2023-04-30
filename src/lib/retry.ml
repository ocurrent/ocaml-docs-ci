open Lwt.Infix
open Lwt.Syntax

let base_sleep_time = 30

let sleep_duration n' =
  (* backoff is based on n *. 30. *. (Float.pow 1.5 n)
     This gives the sequence 0s -> 45s -> 135s -> 300s -> 600s -> 1100s
  *)
  let n = Int.to_float n' in
  let randomised_sleep_time = base_sleep_time + Random.int 20 in
  let backoff = n *. Int.to_float base_sleep_time *. Float.pow 1.5 n in
  Int.to_float randomised_sleep_time +. backoff

let rec retry_loop ?(sleep_duration = sleep_duration) ?job ?(log_string = "")
    ?(number_of_attempts = 0) ?(max_number_of_attempts = 2)
    fn_returning_results_and_retriable_errors =
  let log_line =
    Fmt.str "RETRYING: %s Number of retries: %d" log_string number_of_attempts
  in
  let log_retry =
    match job with
    | Some job -> Current.Job.log job "%s (retriable error condition)" log_line
    | None -> Log.info (fun f -> f "%s (retriable error condition)" log_line)
  in
  let* x = fn_returning_results_and_retriable_errors () in
  match x with
  | Error e ->
      (* Error signals no recovery *)
      Lwt.return_error e
  | Ok (results, []) -> Lwt.return_ok results
  | Ok (_, _errors) when number_of_attempts >= max_number_of_attempts ->
      Lwt.return_error (`Msg "maximum attempts reached")
  | Ok (_, _errors) ->
      (* retry *)
      Lwt_unix.sleep (sleep_duration @@ number_of_attempts) >>= fun () ->
      log_retry;
      retry_loop ~sleep_duration ~log_string
        ~number_of_attempts:(number_of_attempts + 1) ~max_number_of_attempts
        fn_returning_results_and_retriable_errors
