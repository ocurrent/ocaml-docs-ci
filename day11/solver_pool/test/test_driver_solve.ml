(** Standalone repro for the oxcaml profile's driver tool solve.

    Exits 0 on a successful solve, 1 otherwise. Designed to iterate on overlay /
    local-repo / solver tweaks without re-running the whole ocaml-docs-ci
    pipeline — point it at the same three repositories the profile uses and ask
    it to solve [odoc-driver.3.2.0+ox] pinned to [ocaml-variants.5.2.0+ox].

    Run via: dune exec day11/solver_pool/test/test_driver_solve.exe

    Expected success on the current local-repo state: odoc-driver.3.2.0+ox OK (N
    packages in solution) *)

let all_repos =
  [
    "/home/jjl25/ocaml/opam-repository";
    "/home/jjl25/oxcaml/opam-repository";
    "/home/jjl25/local/opam-repository";
  ]

let ocaml_only = [ "/home/jjl25/ocaml/opam-repository" ]
let target = OpamPackage.of_string "odoc-driver.3.1.0"
let ocaml_version = OpamPackage.of_string "ocaml-base-compiler.5.4.1"
let extra_targets = []
let _ = ocaml_only

let resolve_head repos =
  List.map
    (fun path ->
      let sha =
        let cmd = Bos.Cmd.(v "git" % "-C" % path % "rev-parse" % "HEAD") in
        match Bos.OS.Cmd.(run_out cmd |> out_string) with
        | Ok (s, (_, `Exited 0)) -> String.trim s
        | _ -> "HEAD"
      in
      (path, sha))
    repos

let solve_with ~sw env ~label repos =
  Printf.printf "\n=== %s ===\n" label;
  List.iter (fun (p, sha) -> Printf.printf "repo %s @ %s\n" p sha) repos;
  Printf.printf "solving %s pinned to %s ...\n%!"
    (OpamPackage.to_string target)
    (OpamPackage.to_string ocaml_version);
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~ocaml_version
      ~extra_targets ~np:1 ~repos [ target ]
  in
  match List.assoc_opt target results with
  | None ->
      Printf.printf "no result returned\n";
      None
  | Some (Error (msg, _)) ->
      Printf.printf "FAILED\n%s\n" msg;
      None
  | Some (Ok result) ->
      let pkgs =
        result.Day11_solution.Solve_result.build_deps
        |> OpamPackage.Map.keys
        |> List.map OpamPackage.to_string
        |> List.sort String.compare
      in
      Printf.printf "OK — %d packages\n" (List.length pkgs);
      Some pkgs

module S = Set.Make (String)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let a = solve_with ~sw env ~label:"ocaml-only" (resolve_head ocaml_only) in
  let b = solve_with ~sw env ~label:"all-3-repos" (resolve_head all_repos) in
  match (a, b) with
  | Some a, Some b ->
      let sa = S.of_list a and sb = S.of_list b in
      let only_a = S.diff sa sb and only_b = S.diff sb sa in
      let common = S.inter sa sb in
      Printf.printf
        "\n=== diff ===\ncommon: %d\nocaml-only only: %d\nall-3 only: %d\n"
        (S.cardinal common) (S.cardinal only_a) (S.cardinal only_b);
      if not (S.is_empty only_a) then (
        Printf.printf "\npackages picked only in ocaml-only (NOT in all-3):\n";
        S.iter (Printf.printf "  %s\n") only_a);
      if not (S.is_empty only_b) then (
        Printf.printf "\npackages picked only in all-3 (NOT in ocaml-only):\n";
        S.iter (Printf.printf "  %s\n") only_b)
  | _ -> Printf.printf "\ncouldn't compare (one or both solves failed)\n"
