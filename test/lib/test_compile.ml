module Compile = Docs_ci_lib.Compile

let test_extract_hashes_no_retry _switch () =
  let log_lines =
    "compile/u/d5bd534a65ac29b409950ab82ea7ec10/stdlib-shims/0.3.0/page-doc.odoc\n\
    \  compile/u/d5bd534a65ac29b409950ab82ea7ec10/ppx_derivers/1.2.1/1.2.1/\n\
    \  compile/u/d5bd534a65ac29b409950ab82ea7ec10/ppx_derivers/1.2.1/1.2.1/lib/\n\
     compile/u/d5bd534a65ac29b409950ab82ea7ec10/ppx_derivers/1.2.1/1.2.1/lib/ppx_derivers/\n"
  in
  let expected = 0 in
  let result = Compile.extract_hashes ((None, None), []) log_lines in
  Alcotest.(check int) "" expected (List.length (snd result)) |> Lwt.return

let test_extract_hashes_rsync_retry _switch () =
  let log_lines =
    "Warning: Permanently added \
     '[staging.docs.ci.ocaml.org]:2222,[51.158.163.148]:2222' (ECDSA) to the \
     list of known hosts.\n\
    \  ssh: connect to host staging.docs.ci.ocaml.org port 2222: Connection \
     timed out\n\
    \  rsync: connection unexpectedly closed (0 bytes received so far) \
     [Receiver]\n\
    \  rsync error: unexplained error (code 255) at io.c(228) [Receiver=3.2.3]"
  in
  let result = Compile.extract_hashes ((None, None), []) log_lines in
  Alcotest.(check string) "" log_lines (List.hd (snd result)) |> Lwt.return

let test_extract_hashes_several_retry _switch () =
  let log_lines =
    "Warning: Permanently added \
     '[staging.docs.ci.ocaml.org]:2222,[51.158.163.148]:2222' (ECDSA) to the \
     list of known hosts.\n\
    \  ssh: connect to host staging.docs.ci.ocaml.org port 2222: Connection \
     timed out\n\
    \ Temporary failure due to some unknown cause\n\
    \ Could not resolve host\n\
    \      rsync error: unexplained error (code 255) at io.c(228) \
     [Receiver=3.2.3]"
  in
  let result = Compile.extract_hashes ((None, None), []) log_lines in
  Alcotest.(check string) "" log_lines (List.hd (snd result)) |> Lwt.return

let test_extract_hashes_succeeded_no_retry _switch () =
  let log_lines =
    "Warning: Permanently added \
     '[staging.docs.ci.ocaml.org]:2222,[51.158.163.148]:2222' (ECDSA) to the \
     list of known hosts.\n\
    \  ssh: connect to host staging.docs.ci.ocaml.org port 2222: Connection \
     timed out\n\
    \ Temporary failure due to some unknown cause\n\
    \ Could not resolve host\n\
    \      rsync error: unexplained error (code 255) at io.c(228) \
     [Receiver=3.2.3]\n\
    \ Job succeeded"
  in
  let expected = 0 in
  let result = Compile.extract_hashes ((None, None), []) log_lines in
  List.iteri (fun i s -> Printf.printf "%d: %s" i s) (snd result);
  Alcotest.(check int) "" expected (List.length (snd result)) |> Lwt.return

let tests =
  [
    Alcotest_lwt.test_case "extract_hashes_no_retry" `Quick
      test_extract_hashes_no_retry;
    Alcotest_lwt.test_case "extract_hashes_rsync_retry" `Quick
      test_extract_hashes_rsync_retry;
    Alcotest_lwt.test_case "extract_hashes_several_retry" `Quick
      test_extract_hashes_several_retry;
    Alcotest_lwt.test_case "extract_hashes_succeeded_no_retry" `Quick
      test_extract_hashes_succeeded_no_retry;
  ]
