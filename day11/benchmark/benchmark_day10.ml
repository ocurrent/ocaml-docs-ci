(* Benchmark: day10-style filesystem solver for comparison.
   Uses the same Dir_context approach as day10 but standalone. *)

let opam_repository =
  try Sys.getenv "OPAM_REPOSITORY"
  with Not_found -> "/home/jjl25/opam-repository"

let time name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "%-40s %.3fs\n%!" name elapsed;
  result

(* Use day11's solver but with eager package loading to simulate
   day10's filesystem approach *)
let () =
  Printf.printf "=== day10-style solver benchmark ===\n\n";

  (* Eager load: read all packages upfront (like day10 does with readdir) *)
  let git_packages, _store, _commit =
    time "Load opam-repository (git eager)" (fun () ->
        Day11_opam.Git_packages.of_opam_repository opam_repository)
  in

  let opam_env =
    Day11_opam.Opam_env.std_env ~arch:"x86_64" ~os:"linux"
      ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()
  in

  (* The key difference: day10 uses filesystem readdir for each solve,
     day11 uses git objects. Both use opam-0install underneath.
     The solver itself is the same — the difference is package loading. *)
  let _ =
    time "Solve astring.0.8.5 (2nd run)" (fun () ->
        Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
          (OpamPackage.of_string "astring.0.8.5"))
  in

  let _ =
    time "Solve astring.0.8.5 (3rd run)" (fun () ->
        Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
          (OpamPackage.of_string "astring.0.8.5"))
  in

  Printf.printf "\n(day10 uses the same opam-0install solver;\n";
  Printf.printf " the difference is filesystem vs git package loading)\n";
  Printf.printf "\nDone.\n%!"
