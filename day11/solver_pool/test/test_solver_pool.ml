(* Tests for day11_solver_pool. These require day11-solver-worker on PATH
   and OPAM_REPOSITORY pointing to a usable opam-repository git checkout. *)

open Day11_test_util.Test_util

let pkg s = OpamPackage.of_string s

(* Solve a trivial target and verify we get a successful result back
   through the fiber pool. This exercises the whole round trip:
   spawn → await → read JSONL → parse → Solve_result. *)
let test_solve_single () =
  let opam_repo = opam_repository () in
  with_eio @@ fun ~sw env ->
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~np:1
      ~repos:[ (opam_repo, "HEAD") ]
      [ pkg "astring.0.8.5" ]
  in
  Alcotest.(check int) "one result" 1 (List.length results);
  match results with
  | [ (p, Ok _) ] ->
      Alcotest.(check string) "target" "astring.0.8.5" (OpamPackage.to_string p)
  | [ (_, Error (msg, _)) ] ->
      Alcotest.fail (Printf.sprintf "solve failed: %s" msg)
  | _ -> Alcotest.fail "unexpected result shape"

let test_solve_empty () =
  with_eio @@ fun ~sw env ->
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~np:4 ~repos:[] []
  in
  Alcotest.(check int) "no results" 0 (List.length results)

(* Progress callback must be invoked at least once when targets complete. *)
let test_on_progress () =
  let opam_repo = opam_repository () in
  with_eio @@ fun ~sw env ->
  let calls = ref [] in
  let on_progress ~done_count ~total = calls := (done_count, total) :: !calls in
  let targets = [ pkg "astring.0.8.5"; pkg "fmt.0.9.0" ] in
  let _ =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~on_progress ~np:2
      ~repos:[ (opam_repo, "HEAD") ]
      targets
  in
  Alcotest.(check bool) "on_progress was called" true (!calls <> []);
  (* Every call's total equals the number of targets. *)
  List.iter
    (fun (_, t) -> Alcotest.(check int) "total" (List.length targets) t)
    !calls

let test_parallel_np () =
  (* Spawning 4 workers for 4 targets should still return all 4 results,
     with no results lost to fiber races. *)
  let opam_repo = opam_repository () in
  with_eio @@ fun ~sw env ->
  let targets =
    [
      pkg "astring.0.8.5";
      pkg "fmt.0.9.0";
      pkg "fpath.0.7.3";
      pkg "rresult.0.7.0";
    ]
  in
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~np:4
      ~repos:[ (opam_repo, "HEAD") ]
      targets
  in
  Alcotest.(check int)
    "result count matches target count" (List.length targets)
    (List.length results)

let () =
  Alcotest.run "day11_solver_pool"
    [
      ( "Solver_pool",
        [
          Alcotest.test_case "solve_single" `Slow test_solve_single;
          Alcotest.test_case "solve_empty" `Quick test_solve_empty;
          Alcotest.test_case "on_progress" `Slow test_on_progress;
          Alcotest.test_case "parallel np" `Slow test_parallel_np;
        ] );
    ]
