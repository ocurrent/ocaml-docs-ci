(* Solve a single package against a real opam-repository checkout.

   Usage: OPAM_REPOSITORY=~/devel/opam-repository-testing \
            dune exec day11/solver/test/solve_one.exe -- eio.1.3 *)

let () =
  let target =
    if Array.length Sys.argv > 1 then OpamPackage.of_string Sys.argv.(1)
    else failwith "usage: solve_one <pkg.version>"
  in
  let opam_repo =
    match Sys.getenv_opt "OPAM_REPOSITORY" with
    | Some p -> p
    | None -> failwith "OPAM_REPOSITORY not set"
  in
  Printf.printf "Using opam-repository: %s\n%!" opam_repo;
  let packages, _ =
    Day11_opam.Git_packages.of_repositories [ (opam_repo, None) ] in
  let env = Day11_opam.Opam_env.std_env
    ~arch:"arm64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"13" () in
  match Day11_solver.Solve.solve ~packages ~env target with
  | Ok result ->
    let pkgs =
      OpamPackage.Set.elements result.Day11_solution.Solve_result.packages
      |> List.map OpamPackage.to_string in
    Printf.printf "OK (%d packages):\n  %s\n"
      (List.length pkgs) (String.concat " " pkgs);
    let doc =
      OpamPackage.Map.find_opt target
        result.Day11_solution.Solve_result.doc_deps in
    (match doc with
     | Some deps ->
       Printf.printf "doc deps of %s:\n  %s\n"
         (OpamPackage.to_string target)
         (String.concat " "
            (List.map OpamPackage.to_string (OpamPackage.Set.elements deps)))
     | None -> Printf.printf "target not in doc_deps map!\n")
  | Error (msg, _) ->
    Printf.printf "FAIL:\n%s\n" msg;
    exit 1
