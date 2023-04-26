module Retry = Docs_ci_lib.Retry
open Lwt.Infix

let ( let** ) = Lwt_result.bind

let test_simple_success_no_retry _switch () =
  let fn () = Lwt.return_ok (42, []) in
  let expected = 42 in
  Retry.retry_loop ~sleep_duration:(fun _ -> 0.) fn >>= fun r ->
  Alcotest.(check int) "" expected (Result.get_ok r) |> Lwt.return

let test_retry _switch () =
  let counter = ref (-1) in
  let fn () =
    counter := !counter + 1;
    Lwt.return_ok (!counter, [ true ])
  in
  let max_number_of_attempts = 5 in
  let expected = max_number_of_attempts in
  Retry.retry_loop
    ~sleep_duration:(fun _ -> 0.)
    ~log_string:"" ~number_of_attempts:0 ~max_number_of_attempts fn
  >>= fun r -> Alcotest.(check int) "" expected (Result.get_ok r) |> Lwt.return

let test_no_retry _switch () =
  let counter = ref (-1) in
  let fn () =
    counter := !counter + 1;
    Lwt.return_error (`Msg "Error")
  in
  let max_number_of_attempts = 5 in
  let expected = "Error" in
  Retry.retry_loop
    ~sleep_duration:(fun _ -> 0.)
    ~log_string:"" ~number_of_attempts:0 ~max_number_of_attempts fn
  >>= fun r ->
  let (`Msg error_string) = Result.get_error r in
  Alcotest.(check string) "" expected error_string |> Lwt.return

let tests =
  [
    Alcotest_lwt.test_case "simple_no_retry" `Quick test_simple_success_no_retry;
    Alcotest_lwt.test_case "simple_with_retry" `Quick test_retry;
    Alcotest_lwt.test_case "simple_with_error_thus_no_retry" `Quick
      test_no_retry;
  ]
