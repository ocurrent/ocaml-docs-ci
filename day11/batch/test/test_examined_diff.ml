(* Diagnostic: show which packages are examined by some targets but not others. *)

open Day11_test_util.Test_util

let opam_repository =
  Sys.getenv_opt "OPAM_REPOSITORY"
  |> Option.value ~default:"/home/jjl25/opam-repository"

let targets =
  [
    "cohttp-lwt.6.2.1";
    "dream.1.0.0~alpha8";
    "cohttp-async.6.2.1";
    "core.v0.17.1";
    "eio_main.1.2";
    "zarith.1.14";
    "cmdliner.1.3.0";
    "astring.0.8.5";
  ]

let opam_env =
  Day11_opam.Opam_env.std_env ~arch:"x86_64" ~os:"linux"
    ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()

let () =
  if not (is_integration ()) then (
    Printf.printf "Skipping (set DAY11_INTEGRATION=true)\n";
    exit 0);
  let git_packages, _, _ =
    Day11_opam.Git_packages.of_opam_repository opam_repository
  in
  (* Solve each target and collect examined sets *)
  let examined_sets =
    List.filter_map
      (fun target_str ->
        let target = OpamPackage.of_string target_str in
        match
          Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env target
        with
        | Ok result ->
            Some (target_str, result.Day11_solution.Solve_result.examined)
        | Error _ -> None)
      targets
  in
  (* Find the intersection (common to all) *)
  let common =
    match examined_sets with
    | [] -> OpamPackage.Name.Set.empty
    | (_, first) :: rest ->
        List.fold_left
          (fun acc (_, s) -> OpamPackage.Name.Set.inter acc s)
          first rest
  in
  Printf.printf "Common to all: %d packages\n%!"
    (OpamPackage.Name.Set.cardinal common);
  (* For each target, show packages unique to it *)
  List.iter
    (fun (name, examined) ->
      let unique = OpamPackage.Name.Set.diff examined common in
      if not (OpamPackage.Name.Set.is_empty unique) then
        Printf.printf "\n%s: %d unique (beyond %d common):\n  %s\n%!" name
          (OpamPackage.Name.Set.cardinal unique)
          (OpamPackage.Name.Set.cardinal common)
          (String.concat ", "
             (List.map OpamPackage.Name.to_string
                (OpamPackage.Name.Set.elements unique))))
    examined_sets;
  (* Find packages examined by exactly some but not all *)
  let all_examined =
    List.fold_left
      (fun acc (_, s) -> OpamPackage.Name.Set.union acc s)
      OpamPackage.Name.Set.empty examined_sets
  in
  let partial = OpamPackage.Name.Set.diff all_examined common in
  Printf.printf "\nPartially examined (%d packages):\n%!"
    (OpamPackage.Name.Set.cardinal partial);
  OpamPackage.Name.Set.iter
    (fun name ->
      let targets_with =
        List.filter
          (fun (_, s) -> OpamPackage.Name.Set.mem name s)
          examined_sets
      in
      Printf.printf "  %s: %d/%d targets\n%!"
        (OpamPackage.Name.to_string name)
        (List.length targets_with)
        (List.length examined_sets))
    partial
