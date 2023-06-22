module Monitor = Docs_ci_lib.Monitor

let step1 =
  Current_git.clone
    ~schedule:(Current_cache.Schedule.v ())
    ~gref:"main" "https://github.com/ocurrent/ocaml-docs-ci.git"

let step2 =
  Current_git.clone
    ~schedule:(Current_cache.Schedule.v ())
    "https://google.com/"

let step3 : unit Current.t = Current.fail "oh no"

let running =
  Current_docker.Default.build ~level:Current.Level.Dangerous ~pull:false
    `No_context

let fakepkg ~blessing name =
  let open Docs_ci_lib in
  let root = OpamPackage.of_string name in
  let pkg = Package.make ~blacklist:[] ~commit:"0" ~root [] in
  let blessing =
    let set =
      Package.Blessing.Set.v ~counts:(Package.Map.singleton pkg 0) [ pkg ]
    in
    OpamPackage.Map.add root (Current.return set) blessing
  in
  (pkg, blessing)

let pipeline monitor =
  let open Docs_ci_lib in
  let blessing = OpamPackage.Map.empty in
  let pkg, blessing = fakepkg ~blessing "docs-ci.1.0.0" in
  let pkg2, blessing = fakepkg ~blessing "ocurrent.1.1.0" in
  let pkg3, blessing = fakepkg ~blessing "ocluster.0.7.0" in

  let values =
    [
      ( pkg,
        Monitor.(
          Seq
            [
              ("step1", Item step1);
              ("step2", Item step2);
              ( "and-pattern",
                And [ ("sub-step3", Item step1); ("sub-step4", Item step3) ] );
            ]) );
      (pkg2, Monitor.(Seq [ ("fake-running-step", Item running) ]));
      (pkg3, Monitor.(Item step1));
    ]
    |> List.to_seq
    |> Package.Map.of_seq
  in
  let solve_failure =
    [ (OpamPackage.of_string "mirage.4.0.0", "solver failed") ]
  in
  Monitor.(register monitor solve_failure OpamPackage.Map.empty blessing values);
  monitor

let package_step_list_testable =
  Alcotest.testable Monitor.pp_package_steps Monitor.equal_package_steps

let test_lookup_steps_docs_ci _switch () =
  let monitor = pipeline (Monitor.make ()) in
  let result = Monitor.lookup_steps monitor ~name:"docs-ci" |> Result.get_ok in
  let expected =
    [
      {
        Monitor.package = OpamPackage.of_string "docs-ci.1.0.0";
        status = Monitor.Running;
        steps =
          [
            { Monitor.typ = "step1"; job_id = None; status = Monitor.Active };
            { Monitor.typ = "step2"; job_id = None; status = Monitor.Active };
            {
              Monitor.typ = "and-pattern:sub-step3";
              job_id = None;
              status = Monitor.Active;
            };
            {
              Monitor.typ = "and-pattern:sub-step4";
              job_id = None;
              status = Monitor.Err "oh no";
            };
          ];
      };
    ]
  in
  Alcotest.(check (list package_step_list_testable)) "" expected result
  |> Lwt.return

let test_lookup_steps_ocurrent _switch () =
  let monitor = pipeline (Monitor.make ()) in
  let result = Monitor.lookup_steps monitor ~name:"ocurrent" |> Result.get_ok in
  let expected =
    [
      {
        Monitor.package = OpamPackage.of_string "ocurrent.1.1.0";
        status = Monitor.Running;
        steps =
          [
            {
              Monitor.typ = "fake-running-step";
              job_id = None;
              status = Monitor.Active;
            };
          ];
      };
    ]
  in
  Alcotest.(check (list package_step_list_testable)) "" expected result
  |> Lwt.return

let test_lookup_steps_ocluster _switch () =
  let monitor = pipeline (Monitor.make ()) in
  let result = Monitor.lookup_steps monitor ~name:"ocluster" |> Result.get_ok in
  let expected =
    [
      {
        Monitor.package = OpamPackage.of_string "ocluster.0.7.0";
        status = Monitor.Running;
        steps = [ { Monitor.typ = ""; job_id = None; status = Monitor.Active } ];
      };
    ]
  in
  Alcotest.(check (list package_step_list_testable)) "" expected result
  |> Lwt.return

let test_lookup_steps_solve_failure_example _switch () =
  let monitor = pipeline (Monitor.make ()) in
  let result =
    Monitor.lookup_steps monitor ~name:"mirage.4.0.0" |> Result.get_error
  in
  let expected = "no packages found with name: mirage.4.0.0" in
  Alcotest.(check string) "" expected result |> Lwt.return

let tests =
  [
    Alcotest_lwt.test_case "simple_lookup_steps_example_1" `Quick
      test_lookup_steps_docs_ci;
    Alcotest_lwt.test_case "simple_lookup_steps_example_2" `Quick
      test_lookup_steps_ocurrent;
    Alcotest_lwt.test_case "simple_lookup_steps_example_3" `Quick
      test_lookup_steps_ocluster;
    Alcotest_lwt.test_case "simple_lookup_steps_example_4" `Quick
      test_lookup_steps_solve_failure_example;
  ]

let () = Lwt_main.run @@ Alcotest_lwt.run "test_lib" [ ("monitor", tests) ]
