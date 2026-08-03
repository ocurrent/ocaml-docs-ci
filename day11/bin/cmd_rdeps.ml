(** rdeps command: find reverse dependencies *)

open Cmdliner

let run profile_name profile_dir package =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (profile, _paths) -> (
      let _git_packages, repos_with_shas, _opam_env =
        Common.setup_solver profile.opam_repositories
      in
      let pkg = OpamPackage.of_string package in
      let results =
        Common.with_eio @@ fun ~sw env ->
        Day11_solver_pool.Solver_pool.solve_many ~sw env ~np:1
          ~repos:repos_with_shas [ pkg ]
      in
      match List.assoc_opt pkg results with
      | Some (Error (diag, _examined)) ->
          Printf.eprintf "Cannot solve %s: %s\n" package diag;
          1
      | None ->
          Printf.eprintf "No result for %s\n" package;
          1
      | Some (Ok result) ->
          let rdeps =
            Day11_solution.Rdeps.find
              [ result.Day11_solution.Solve_result.build_deps ]
              pkg
          in
          if OpamPackage.Set.is_empty rdeps then
            Printf.printf "No reverse dependencies for %s\n" package
          else (
            Printf.printf "Reverse dependencies of %s:\n" package;
            OpamPackage.Set.iter
              (fun p -> Printf.printf "  %s\n" (OpamPackage.to_string p))
              rdeps);
          0)

let package_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"PACKAGE" ~doc:"Package to find rdeps for")

let cmd =
  let info = Cmd.info "rdeps" ~doc:"Find reverse dependencies" in
  let term =
    Term.(
      const run $ Common.profile_term $ Common.profile_dir_term $ package_term)
  in
  Cmd.v info term
