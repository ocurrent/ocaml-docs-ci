(** Which target pulls odoc.3.1.0+ox into its solution? *)

let repos =
  [
    "/home/jjl25/ocaml/opam-repository";
    "/home/jjl25/oxcaml/opam-repository";
    "/home/jjl25/local/opam-repository";
  ]

let ocaml_version = OpamPackage.of_string "ocaml-variants.5.2.0+ox"

(* The same 10 targets as profile oxcaml-small — expand to all versions. *)
let target_names =
  [
    "fmt";
    "logs";
    "fpath";
    "bos";
    "astring";
    "cmdliner";
    "yojson";
    "ppxlib";
    "eio";
    "cstruct";
  ]

let odoc_310 = OpamPackage.of_string "odoc.3.1.0+ox"

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let repos_with_shas =
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
  in
  let env = (env :> Eio_unix.Stdenv.base) in
  (* Enumerate all versions of each target name by querying the repos. *)
  let packages, _ =
    Day11_opam.Git_packages.of_repositories
      (List.map (fun r -> (r, None)) repos)
  in
  let all_versions name =
    try
      Day11_opam.Git_packages.get_versions packages
        (OpamPackage.Name.of_string name)
      |> OpamPackage.Version.Map.keys
      |> List.map (fun v ->
          OpamPackage.create (OpamPackage.Name.of_string name) v)
    with _ -> []
  in
  let targets = List.concat_map all_versions target_names in
  Printf.printf "total targets: %d (across %d names)\n%!" (List.length targets)
    (List.length target_names);
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~ocaml_version ~np:8
      ~repos:repos_with_shas targets
  in
  let pulls_in_odoc310 =
    List.filter_map
      (fun (t, r) ->
        match r with
        | Ok sr ->
            if
              OpamPackage.Map.mem odoc_310
                sr.Day11_solution.Solve_result.build_deps
            then
              let patches =
                OpamPackage.Map.bindings
                  sr.Day11_solution.Solve_result.build_deps
                |> List.filter (fun (p, _) ->
                    OpamPackage.Name.to_string (OpamPackage.name p)
                    = "oxcaml-odoc-patches")
                |> List.map (fun (p, _) ->
                    OpamPackage.version p |> OpamPackage.Version.to_string)
                |> String.concat ","
              in
              Some (OpamPackage.to_string t, patches)
            else None
        | Error _ -> None)
      results
  in
  Printf.printf "\ntargets whose solution includes %s (%d):\n"
    (OpamPackage.to_string odoc_310)
    (List.length pulls_in_odoc310);
  List.iter
    (fun (t, patches) ->
      Printf.printf "  %s (oxcaml-odoc-patches: %s)\n" t
        (if patches = "" then "<none>" else patches))
    pulls_in_odoc310
