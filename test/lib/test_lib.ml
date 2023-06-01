let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "test_lib"
       [ ("retry", Test_retry.tests); ("compile", Test_compile.tests) ]
