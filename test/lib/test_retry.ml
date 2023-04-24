module Misc = Docs_ci_lib.Misc
open Lwt.Infix

let ( let** ) = Lwt_result.bind

let test_simple_success_no_retry _switch () =
  let fn () = Lwt.return_ok (42, []) in
  let expected = 42 in
  Misc.Retry.retry_loop ~sleep_duration:(fun _ -> 0.) fn >>= fun r ->
  Alcotest.(check int) "foo" expected (Result.get_ok r) |> Lwt.return

let test_retry _switch () =
  let counter = ref (-1) in
  let fn () =
    counter := !counter + 1;
    Lwt.return_ok (!counter, [ true ])
  in
  let max_number_of_attempts = 5 in
  let expected = max_number_of_attempts + 1 in
  (* the counter is incremented on the last execution of the function *)
  Misc.Retry.retry_loop
    ~sleep_duration:(fun _ -> 0.)
    ~log_string:"" ~number_of_attempts:0 ~max_number_of_attempts fn
  >>= fun r ->
  Alcotest.(check int) "foo" expected (Result.get_ok r) |> Lwt.return

let tests =
  [
    Alcotest_lwt.test_case "simple" `Quick test_simple_success_no_retry;
    Alcotest_lwt.test_case "simple" `Quick test_retry;
  ]
