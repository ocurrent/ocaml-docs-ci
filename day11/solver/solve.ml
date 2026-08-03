module Solver = Opam_0install.Solver.Make (Context)

(** Extract package names from the [x-extra-doc-deps] extension field. *)
let get_extra_doc_deps opamfile =
  let open OpamParserTypes.FullPos in
  let extensions = OpamFile.OPAM.extensions opamfile in
  match OpamStd.String.Map.find_opt "x-extra-doc-deps" extensions with
  | None -> OpamPackage.Name.Set.empty
  | Some value ->
      let extract_name item =
        match item.pelem with
        | String name -> Some name
        | Option (inner, _) -> (
            match inner.pelem with String name -> Some name | _ -> None)
        | _ -> None
      in
      let extract_names acc v =
        match v.pelem with
        | List { pelem = items; _ } ->
            List.fold_left
              (fun acc item ->
                match extract_name item with
                | Some name ->
                    OpamPackage.Name.Set.add
                      (OpamPackage.Name.of_string name)
                      acc
                | None -> acc)
              acc items
        | _ -> acc
      in
      extract_names OpamPackage.Name.Set.empty value

let solve_internal ~packages:pkgs ~env
    ?(constraints = OpamPackage.Name.Map.empty)
    ?(pins = OpamPackage.Name.Map.empty) ?(prefer_oldest = false) ?(doc = true)
    ?(extra_targets = []) ?(pin_target = true) target =
  let name = OpamPackage.name target in
  let version = OpamPackage.version target in
  let constraints =
    if pin_target then
      match OpamPackage.Name.Map.find_opt name constraints with
      | Some (`Eq, existing)
        when not (OpamPackage.Version.equal existing version) ->
          (* Caller asked us to pin the target but a conflicting [=]
           constraint is already set. Bail out below. *)
          constraints
      | _ -> OpamPackage.Name.Map.add name (`Eq, version) constraints
    else
      (* Latest-mode solve: don't add an [=] constraint on the target
         — the solver picks any version that satisfies the rest of
         the universe. The [version] field on [target] is treated as
         a hint only (it appears in error messages and as the input
         "I'm asking about this package"); the result may have a
         different version. *)
      constraints
  in
  match OpamPackage.Name.Map.find_opt name constraints with
  | Some (`Eq, existing)
    when pin_target && not (OpamPackage.Version.equal existing version) ->
      let msg =
        Printf.sprintf "Target %s conflicts with constraint %s = %s"
          (OpamPackage.to_string target)
          (OpamPackage.Name.to_string name)
          (OpamPackage.Version.to_string existing)
      in
      Error (msg, OpamPackage.Name.Set.empty)
  | _ -> (
      let constraints =
        List.fold_left
          (fun acc et ->
            OpamPackage.Name.Map.add (OpamPackage.name et)
              (`Eq, OpamPackage.version et)
              acc)
          constraints extra_targets
      in
      let context =
        Context.create ~prefer_oldest ~constraints ~pins ~env ~packages:pkgs
          ~doc ()
      in
      let compiler_root =
        let compiler_names =
          List.map OpamPackage.Name.of_string
            [ "ocaml-base-compiler"; "ocaml-variants"; "ocaml-system" ]
        in
        List.find_opt
          (fun n -> OpamPackage.Name.Map.mem n constraints)
          compiler_names
      in
      (* Include x-extra-doc-deps as additional roots so they end up in the
     solution for cross-referencing during doc generation.
     Skip when doc=false (tool builds don't need doc deps). *)
      let extra_doc_roots =
        if not doc then []
        else
          let opam =
            match OpamPackage.Name.Map.find_opt name pins with
            | Some (_ver, opam) -> opam
            | None -> (
                try Day11_opam.Git_packages.get_package pkgs target
                with Not_found -> OpamFile.OPAM.empty)
          in
          let open OpamParserTypes.FullPos in
          let extensions = OpamFile.OPAM.extensions opam in
          match OpamStd.String.Map.find_opt "x-extra-doc-deps" extensions with
          | None -> []
          | Some value ->
              let extract_name (item : OpamParserTypes.FullPos.value) =
                match item.pelem with
                | String s -> Some (OpamPackage.Name.of_string s)
                | Option ({ pelem = String s; _ }, _) ->
                    Some (OpamPackage.Name.of_string s)
                | _ -> None
              in
              let names =
                match value.pelem with
                | List { pelem = items; _ } ->
                    List.filter_map extract_name items
                | _ -> []
              in
              (* Drop doc-pipeline tools: they're mounted from the
         pre-built tool layer at doc-build time, not from this
         solve. Including them here puts their full closure
         (cmdliner / yojson / eio / progress) into the target's
         doc-deps, which keys the build-dag universe and
         multiplies per-universe rebuilds. *)
              List.filter
                (fun n -> not (Day11_solution.Tool_names.is_tool_only n))
                names
      in
      let extra_target_roots = List.map OpamPackage.name extra_targets in
      let roots =
        (match compiler_root with Some c -> [ c ] | None -> [])
        @ [ name ]
        @ extra_doc_roots
        @ extra_target_roots
      in
      match Solver.solve context roots with
      | Error e ->
          let examined = Context.examined_packages context in
          Error (Solver.diagnostics ~verbose:true e, examined)
      | Ok selections ->
          let examined = Context.examined_packages context in
          let solved_pkgs = Solver.packages_of_result selections in
          let solved_names =
            List.fold_left
              (fun acc p -> OpamPackage.Name.Set.add (OpamPackage.name p) acc)
              OpamPackage.Name.Set.empty solved_pkgs
          in
          let compute_deps ~doc ~post ~extra_doc =
            let ctx = Context.with_doc_post ~doc ~post context in
            List.fold_left
              (fun acc pkg ->
                let opam =
                  match
                    OpamPackage.Name.Map.find_opt (OpamPackage.name pkg) pins
                  with
                  | Some (_ver, opam) -> opam
                  | None -> (
                      try Day11_opam.Git_packages.get_package pkgs pkg
                      with Not_found -> OpamFile.OPAM.empty)
                in
                let deps =
                  Context.filter_deps ctx pkg (OpamFile.OPAM.depends opam)
                in
                let dep_names =
                  OpamFormula.fold_left
                    (fun acc (dep_name, _) ->
                      OpamPackage.Name.Set.add dep_name acc)
                    OpamPackage.Name.Set.empty deps
                in
                let depopts = OpamFile.OPAM.depopts opam in
                let depopt_names =
                  OpamFormula.fold_left
                    (fun acc (dep_name, _) ->
                      if OpamPackage.Name.Set.mem dep_name solved_names then
                        OpamPackage.Name.Set.add dep_name acc
                      else acc)
                    OpamPackage.Name.Set.empty depopts
                in
                let all_dep_names =
                  OpamPackage.Name.Set.union dep_names depopt_names
                in
                (* When computing doc deps, also include per-package
             x-extra-doc-deps (intersected with the solved set).
             Tool-only names are NOT filtered here: if a tool made it
             into the solution (e.g. odoc's own extras when odoc is a
             real dep of the target), it is a genuine link-graph dep —
             dropping it would collapse the build/doc dep distinction
             that {!Day11_doc.Doc_deps.needs_separate_link} keys on.
             Tool universes are kept small by dropping tool-only names
             from the solve ROOTS above, not from dep edges. *)
                let all_dep_names =
                  if extra_doc then
                    let extra = get_extra_doc_deps opam in
                    let extra_in_solution =
                      OpamPackage.Name.Set.inter extra solved_names
                    in
                    OpamPackage.Name.Set.union all_dep_names extra_in_solution
                  else all_dep_names
                in
                let dep_pkgs =
                  List.filter
                    (fun p ->
                      OpamPackage.Name.Set.mem (OpamPackage.name p)
                        all_dep_names)
                    solved_pkgs
                  |> OpamPackage.Set.of_list
                in
                OpamPackage.Map.add pkg dep_pkgs acc)
              OpamPackage.Map.empty solved_pkgs
          in
          let build_deps =
            compute_deps ~doc:false ~post:false ~extra_doc:false
          in
          let doc_deps = compute_deps ~doc:true ~post:true ~extra_doc:true in
          let packages = OpamPackage.Set.of_list solved_pkgs in
          Ok
            ( {
                Day11_solution.Solve_result.packages;
                build_deps;
                doc_deps;
                examined;
              },
              examined ))

let add_ocaml_constraint ?ocaml_version constraints =
  match ocaml_version with
  | Some pkg -> (
      let constraints =
        OpamPackage.Name.Map.add (OpamPackage.name pkg)
          (`Eq, OpamPackage.version pkg)
          constraints
      in
      (* Pin the [ocaml] virtual to the matching major.minor.patch
       (stripping [+ox] / [~rc1] suffixes). Without this, a single-
       target tool solve leaves [ocaml] free and opam picks the
       highest-ranked match of [ocaml >= ...]. *)
      let ver_str = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let base_ver =
        let stop_at c =
          try String.index ver_str c with Not_found -> String.length ver_str
        in
        let cut = min (stop_at '+') (stop_at '~') in
        String.sub ver_str 0 cut
      in
      try
        let v = OpamPackage.Version.of_string base_ver in
        OpamPackage.Name.Map.add
          (OpamPackage.Name.of_string "ocaml")
          (`Eq, v) constraints
      with _ -> constraints)
  | None ->
      OpamPackage.Name.Map.add
        (OpamPackage.Name.of_string "ocaml-base-compiler")
        (`Geq, OpamPackage.Version.of_string "4.08.0")
        constraints

let solve ~packages ~env ?constraints ?pins ?prefer_oldest ?(doc = true)
    ?(extra_targets = []) ?(pin_target = true) ?ocaml_version target =
  let constraints =
    add_ocaml_constraint ?ocaml_version
      (Option.value ~default:OpamPackage.Name.Map.empty constraints)
  in
  match
    solve_internal ~packages ~env ~constraints ?pins ?prefer_oldest ~doc
      ~extra_targets ~pin_target target
  with
  | Ok (result, _examined) -> Ok result
  | Error (msg, examined) -> Error (msg, examined)
