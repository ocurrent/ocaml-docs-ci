let src = Logs.Src.create "day11.build.tools" ~doc:"Tool building"
module Log = (val Logs.src_log src)

module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
type build = Build.t

let read_pins_from_dir dir =
  let opam_files = Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".opam") in
  List.fold_left (fun acc filename ->
    let name = Filename.chop_suffix filename ".opam" in
    let path = Filename.concat dir filename in
    try
      let opam = OpamFile.OPAM.read
        (OpamFile.make (OpamFilename.raw path)) in
      OpamPackage.Name.Map.add
        (OpamPackage.Name.of_string name)
        (OpamPackage.Version.of_string "dev", opam) acc
    with _ -> acc
  ) OpamPackage.Name.Map.empty opam_files

let source_dir_strategy pkg =
  let pkg_str = OpamPackage.to_string pkg in
  { Types.cmd = Printf.sprintf
      "opam-build -v %s --source-dir /home/opam/src"
      pkg_str;
    cleanup = Build_layer.opam_build_cleanup }

(* DAG-planning half of {!plan_tool}: everything after the solve.
   Split out so a caller holding a cached [Solve_result] (see
   {!Day11_batch.Incremental_solver.tool_solutions_dirname}) can skip
   the solver-worker subprocess entirely. *)
let plan_tool_of_result (benv : Types.build_env) ~packages
    ?(source_dirs = OpamPackage.Name.Map.empty)
    ?cache
    target (result : Day11_solution.Solve_result.t) =
  let pkg_str = OpamPackage.to_string target in
  let solution = result.build_deps in
  let doc_solution = result.doc_deps in
  let cache = match cache with
    | Some c -> c
    | None ->
      let find_opam = Day11_opam.Git_packages.find_package packages in
      Hash_cache.create ~find_opam ()
  in
  let nodes = Dag.build_dag cache ~base_hash:benv.base.hash
    [ (target, solution, doc_solution) ] in
  let last = List.find (fun (n : build) ->
    OpamPackage.equal n.pkg target) nodes in
  let tool_dir = Build.dir ~os_dir:benv.os_dir last in
  Log.info (fun m -> m "Tool %s: %d nodes in DAG"
    pkg_str (List.length nodes));
  Ok ({ Tool.hash = last.hash; dir = tool_dir;
        builds = nodes },
      source_dirs)

(* Solver half of {!plan_tool}: one solver-worker subprocess for one
   target. Exposed so callers can interpose a solution cache between
   solving and planning. *)
let solve_tool ~sw env ~repos
    ?(constraints = [])
    ?(pin_dirs = [])
    ?(doc = true)
    ?ocaml_version
    target =
  let pkg_str = OpamPackage.to_string target in
  Log.info (fun m -> m "Planning tool %s" pkg_str);
  let results = Day11_solver_pool.Solver_pool.solve_many ~sw env
    ~pin_dirs ~constraints ~doc ?ocaml_version
    ~np:1 ~repos [ target ] in
  match List.assoc_opt target results with
  | None ->
      Rresult.R.error_msgf "Cannot solve %s: no result" pkg_str
  | Some (Error (diag, _examined)) ->
      Rresult.R.error_msgf "Cannot solve %s: %s" pkg_str diag
  | Some (Ok result) -> Ok result

let plan_tool ~sw env (benv : Types.build_env) ~packages ~repos
    ?(constraints = [])
    ?(pin_dirs = [])
    ?(doc = true)
    ?ocaml_version
    ?(source_dirs = OpamPackage.Name.Map.empty)
    ?cache
    target =
  match solve_tool ~sw env ~repos ~constraints ~pin_dirs ~doc
          ?ocaml_version target with
  | Error _ as e -> e
  | Ok result ->
    plan_tool_of_result benv ~packages ~source_dirs ?cache target result

let build_tool ~sw env (benv : Types.build_env) ?(np = 4) ~packages ~repos
    ?(constraints = [])
    ?(pin_dirs = [])
    ?(doc = true)
    ?ocaml_version
    ?(source_dirs = OpamPackage.Name.Map.empty)
    ?(mounts = []) target =
  let pkg_str = OpamPackage.to_string target in
  Log.info (fun m -> m "Building tool %s" pkg_str);
  match plan_tool ~sw env benv ~packages ~repos
          ~constraints ~pin_dirs ~doc ?ocaml_version
          ~source_dirs target with
  | Error _ as e -> e
  | Ok (tool, source_dirs) ->
      let nodes = tool.builds in
      (* Repo source paths for the per-package slice. [Build_layer.build]
         (via [Container_backend]) extracts just [node.pkg] from these
         into a fresh repo mounted at [~/.opam/repo/default], shadowing
         the full opam-repository baked into the base image. Without
         this, opam loads switch state against all ~18k packages
         (several seconds) on every tool node. Builds already pass this;
         tool nodes previously didn't, hence the slow tool jobs. *)
      let opam_repositories = List.map (fun (p, _sha) -> Fpath.v p) repos in
      Printf.printf "  Solution packages:\n%!";
      List.iter (fun (node : Day11_opam_layer.Build.t) ->
        Printf.printf "    %s (%d deps)\n%!"
          (OpamPackage.to_string node.pkg) (List.length node.deps)
      ) nodes;
      let failed = ref None in
      Dag_executor.execute env ~np
        ~on_complete:(fun ~stats ~cached:_ node success ->
          Printf.printf "  [%d/%d, %d ok, %d failed] %s: %s\n%!"
            stats.completed stats.total stats.ok stats.failed
            (OpamPackage.to_string node.pkg)
            (if success then "OK" else "FAIL");
          if not success && !failed = None then
            failed := Some (OpamPackage.to_string node.pkg))
        ~on_cascade:(fun ~failed:f ~failed_dep ->
          Printf.printf "  CASCADE: %s (dep %s failed)\n%!"
            (OpamPackage.to_string f.pkg)
            (OpamPackage.to_string failed_dep.pkg);
          if !failed = None then
            failed := Some (OpamPackage.to_string f.pkg))
        nodes
        (fun node ->
          let name = OpamPackage.name node.pkg in
          match OpamPackage.Name.Map.find_opt name source_dirs with
          | Some dir ->
            let src_mount = Day11_container.Mount.bind_ro
              ~src:dir "/home/opam/src" in
            let strategy = source_dir_strategy node.pkg in
            (match Build_layer.build ~sw env benv
                     ~mounts:(src_mount :: mounts) ~opam_repositories node
                     ~strategy () with
             | Types.Success _ -> true
             | _ -> false)
          | None ->
            (match Build_layer.build ~sw env benv ~mounts ~opam_repositories node () with
             | Types.Success _ -> true
             | _ -> false));
      match !failed with
      | Some name ->
          Rresult.R.error_msgf "Build failed for %s" name
      | None -> Ok tool

let build_tool_from_repo ~sw env benv ?(np = 4) ~packages ~repos
    ?ocaml_version ?(mounts = []) ?(extra_repo_dirs = [])
    ?(extra_target_names = [])
    ~repo_dir ~target_name () =
  let all_dirs = repo_dir :: extra_repo_dirs in
  let source_dirs = List.fold_right (fun dir acc ->
    let opam_files = Sys.readdir dir |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".opam") in
    List.fold_left (fun acc filename ->
      let name = Filename.chop_suffix filename ".opam" in
      OpamPackage.Name.Map.add
        (OpamPackage.Name.of_string name) dir acc
    ) acc opam_files
  ) all_dirs OpamPackage.Name.Map.empty in
  let target = OpamPackage.of_string (target_name ^ ".dev") in
  let constraints = List.map (fun n ->
    OpamPackage.of_string (n ^ ".dev")) extra_target_names in
  build_tool ~sw env benv ~np ~packages ~repos ~pin_dirs:all_dirs
    ~constraints ~source_dirs ~doc:false ?ocaml_version ~mounts target
