(* Standalone solver worker.
   Reads opam-repository at a given commit, solves multiple packages,
   writes each result as one JSON object per line to an output file.

   Usage: solver_worker --repo PATH[:SHA] ... --output FILE
          [--pin-dir DIR] ... [--constraint NAME.VERSION] ...
          [--no-doc] [--arch A] [--os O] [--os-distribution D]
          [--os-family F] [--os-version V]
          PKG1 PKG2 ... *)

open Cmdliner

let repo_conv =
  let parse s =
    match String.split_on_char ':' s with
    | [ path ] -> Ok (path, None)
    | path :: rest -> Ok (path, Some (String.concat ":" rest))
    | [] -> Error (`Msg "empty repo spec")
  in
  let pp fmt (path, sha) =
    match sha with
    | None -> Format.pp_print_string fmt path
    | Some s -> Format.fprintf fmt "%s:%s" path s
  in
  Arg.conv (parse, pp)

let repo_term =
  let doc =
    "opam-repository path with optional commit SHA (repeatable, layered in \
     order)"
  in
  Arg.(
    non_empty & opt_all repo_conv [] & info [ "repo" ] ~docv:"PATH[:SHA]" ~doc)

let output_term =
  let doc = "Output file (one JSON per line); defaults to stdout" in
  Arg.(value & opt (some string) None & info [ "output" ] ~docv:"FILE" ~doc)

let ocaml_version_term =
  let doc = "Compiler version (e.g. ocaml-base-compiler.5.2.1)" in
  Arg.(
    value & opt (some string) None & info [ "ocaml-version" ] ~docv:"PKG" ~doc)

let pin_dir_term =
  let doc = "Directory of .opam files to pin at version dev (repeatable)" in
  Arg.(value & opt_all string [] & info [ "pin-dir" ] ~docv:"DIR" ~doc)

let constraint_term =
  let doc = "Pin package at exact version, e.g. NAME.VERSION (repeatable)" in
  Arg.(
    value & opt_all string [] & info [ "constraint" ] ~docv:"NAME.VERSION" ~doc)

let extra_target_term =
  let doc =
    "Additional root at exact version, e.g. NAME.VERSION. Like --constraint, \
     but also adds to the solve roots so the solver must install it (not just \
     constrain its version if it ends up in the solution). Repeatable."
  in
  Arg.(
    value
    & opt_all string []
    & info [ "extra-target" ] ~docv:"NAME.VERSION" ~doc)

let no_doc_term =
  let doc = "Disable doc dependencies" in
  Arg.(value & flag & info [ "no-doc" ] ~doc)

let no_pin_target_term =
  let doc =
    "Don't pin each target to its given version. Use when the target version \
     on the command line is just a hint and the solver should be free to pick \
     another version (e.g. an oxcaml [+ox] variant) when the rest of the \
     universe doesn't fit the nominal version."
  in
  Arg.(value & flag & info [ "no-pin-target" ] ~doc)

let arch_term =
  let doc = "Architecture (default x86_64)" in
  Arg.(value & opt string "x86_64" & info [ "arch" ] ~docv:"ARCH" ~doc)

let os_term =
  let doc = "Operating system (default linux)" in
  Arg.(value & opt string "linux" & info [ "os" ] ~docv:"OS" ~doc)

let os_distribution_term =
  let doc = "Distribution (default debian)" in
  Arg.(
    value & opt string "debian" & info [ "os-distribution" ] ~docv:"DIST" ~doc)

let os_family_term =
  let doc = "OS family (default debian)" in
  Arg.(value & opt string "debian" & info [ "os-family" ] ~docv:"FAM" ~doc)

let os_version_term =
  let doc = "OS version (default 12)" in
  Arg.(value & opt string "12" & info [ "os-version" ] ~docv:"VER" ~doc)

let targets_term =
  let doc = "Package(s) to solve" in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"PKG" ~doc)

let read_pins_from_dir dir =
  let opam_files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".opam")
  in
  List.fold_left
    (fun acc filename ->
      let name = Filename.chop_suffix filename ".opam" in
      let path = Filename.concat dir filename in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        OpamPackage.Name.Map.add
          (OpamPackage.Name.of_string name)
          (OpamPackage.Version.of_string "dev", opam)
          acc
      with _ -> acc)
    OpamPackage.Name.Map.empty opam_files

let parse_constraints constraint_strs =
  List.fold_left
    (fun acc s ->
      let pkg = OpamPackage.of_string s in
      OpamPackage.Name.Map.add (OpamPackage.name pkg)
        (`Eq, OpamPackage.version pkg)
        acc)
    OpamPackage.Name.Map.empty constraint_strs

let examined_to_json examined =
  `List
    (OpamPackage.Name.Set.fold
       (fun n acc -> `String (OpamPackage.Name.to_string n) :: acc)
       examined [])

let solve_one ~packages ~env ~pins ~constraints ~extra_targets ~doc ~pin_target
    ?ocaml_version pkg =
  match
    Day11_solver.Solve.solve ~packages ~env ~pins ~constraints ~extra_targets
      ~doc ~pin_target ?ocaml_version pkg
  with
  | Ok result ->
      `Assoc
        [
          ("package", `String (OpamPackage.to_string pkg));
          ("result", Day11_solution.Solve_result.to_json result);
        ]
  | Error (msg, examined) ->
      `Assoc
        [
          ("failed", `Bool true);
          ("package", `String (OpamPackage.to_string pkg));
          ("error", `String msg);
          ("examined", examined_to_json examined);
        ]

let run repo_list output ocaml_version pin_dirs constraint_strs
    extra_target_strs no_doc no_pin_target arch os os_distribution os_family
    os_version targets =
  let packages, _repos_with_shas =
    Day11_opam.Git_packages.of_repositories repo_list
  in
  let env =
    Day11_opam.Opam_env.std_env ~arch ~os ~os_distribution ~os_family
      ~os_version ()
  in
  let oc = match output with None -> stdout | Some path -> open_out path in
  let ocaml_version = Option.map OpamPackage.of_string ocaml_version in
  let pins =
    List.fold_left
      (fun acc dir ->
        OpamPackage.Name.Map.union (fun _a b -> b) acc (read_pins_from_dir dir))
      OpamPackage.Name.Map.empty pin_dirs
  in
  let constraints = parse_constraints constraint_strs in
  let extra_targets = List.map OpamPackage.of_string extra_target_strs in
  let doc = not no_doc in
  let pin_target = not no_pin_target in
  List.iter
    (fun target_str ->
      let pkg = OpamPackage.of_string target_str in
      let json =
        solve_one ~packages ~env ~pins ~constraints ~extra_targets ~doc
          ~pin_target ?ocaml_version pkg
      in
      output_string oc (Yojson.Safe.to_string json);
      output_char oc '\n';
      flush oc)
    targets;
  if output <> None then close_out oc

let cmd =
  let doc = "Solve packages and write solutions as JSON lines" in
  let info = Cmd.info "solver_worker" ~doc in
  Cmd.v info
    Term.(
      const run
      $ repo_term
      $ output_term
      $ ocaml_version_term
      $ pin_dir_term
      $ constraint_term
      $ extra_target_term
      $ no_doc_term
      $ no_pin_target_term
      $ arch_term
      $ os_term
      $ os_distribution_term
      $ os_family_term
      $ os_version_term
      $ targets_term)

let () = exit (Cmd.eval cmd)
