(* Trial run: simulate watching opam-repository across commits.
   For each commit, solve + build latest versions of packages.
   Uses incremental solver reuse where possible.

   Usage: trial_run [--repo PATH] [--cache-dir PATH]
          [--ocaml-version PKG] [--since DATE] [--until DATE]
          [--np N] [--packages FILE | PKG1 PKG2 ...] *)

let opam_repository = ref (
  try Sys.getenv "OPAM_REPOSITORY"
  with Not_found -> "/home/jjl25/opam-repository")

let scratch_cache = ref (
  try Sys.getenv "CACHE_DIR"
  with Not_found -> "/tmp/day11-trial")

let ocaml_version_str = ref ""
let since = ref "2026-02-01"
let until = ref "2026-03-08"
let np = ref 4
let packages_file = ref ""
let extra_packages = ref []

let default_packages = [
  "astring"; "fmt"; "fpath"; "rresult"; "bos"; "logs"; "cmdliner";
  "ptime"; "uutf"; "result"; "mtime"; "hmap"; "psq"; "cstruct";
  "bigstringaf"; "angstrom"; "domain-local-await"; "optint"; "topkg";
  "re"; "yojson"; "jsonm"; "ppxlib"; "sexplib0"; "base64"; "csexp";
  "lwt"; "eio"; "tyxml"; "brr"; "decompress"; "checkseum"; "cmarkit";
  "progress"; "terminal"; "js_of_ocaml"; "js_of_ocaml-compiler";
  "ocamlgraph"; "odoc"; "odoc-parser"; "sqlite3"; "syndic"; "ipaddr";
  "dns"; "cohttp"; "tls"; "mirage-crypto"; "zarith"; "num";
]

let spec = [
  "--repo", Arg.Set_string opam_repository,
    "PATH opam-repository path (or OPAM_REPOSITORY env)";
  "--cache-dir", Arg.Set_string scratch_cache,
    "PATH cache directory (or CACHE_DIR env)";
  "--ocaml-version", Arg.Set_string ocaml_version_str,
    "PKG compiler version (e.g. ocaml-base-compiler.5.2.1 or ocaml-variants.5.2.0+ox)";
  "--since", Arg.Set_string since,
    "DATE start date for commits (default: 2026-02-01)";
  "--until", Arg.Set_string until,
    "DATE end date for commits (default: 2026-03-08)";
  "--np", Arg.Set_int np,
    "N number of parallel solver workers (default: 4)";
  "--packages", Arg.Set_string packages_file,
    "FILE file with one package name per line";
]

let time name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "  %-40s %.3fs\n%!" name elapsed;
  result

let find_latest_version git_packages name =
  let versions = Day11_opam.Git_packages.get_versions git_packages
    (OpamPackage.Name.of_string name) in
  let non_avoided =
    OpamPackage.Version.Map.filter (fun _v opam ->
      not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam)
    ) versions in
  let versions = if OpamPackage.Version.Map.is_empty non_avoided
    then versions else non_avoided in
  match OpamPackage.Version.Map.max_binding_opt versions with
  | Some (v, _) ->
    Some (OpamPackage.create (OpamPackage.Name.of_string name) v)
  | None -> None

let load_packages_from_file path =
  let ic = open_in path in
  let pkgs = ref [] in
  (try while true do
    let line = String.trim (input_line ic) in
    if line <> "" && not (String.starts_with ~prefix:"#" line) then
      pkgs := line :: !pkgs
  done with End_of_file -> ());
  close_in ic;
  List.rev !pkgs

let () =
  Arg.parse spec (fun s -> extra_packages := s :: !extra_packages)
    "trial_run: simulate watching opam-repository";
  let opam_repository = !opam_repository in
  let scratch_cache = Fpath.v !scratch_cache in
  let ocaml_version =
    if !ocaml_version_str = "" then None
    else Some (OpamPackage.of_string !ocaml_version_str) in
  let package_names =
    if !packages_file <> "" then load_packages_from_file !packages_file
    else if !extra_packages <> [] then List.rev !extra_packages
    else default_packages in
  let np = !np in
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  Printf.printf "=== Trial run: %d packages across opam-repo commits ===\n"
    (List.length package_names);
  Printf.printf "  repo: %s\n" opam_repository;
  (match ocaml_version with
   | Some v -> Printf.printf "  compiler: %s\n" (OpamPackage.to_string v)
   | None -> Printf.printf "  compiler: latest ocaml-base-compiler >= 4.08\n");
  Printf.printf "  range: %s to %s\n\n%!" !since !until;
  Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  (* Setup base image *)
  let base = match Day11_opam_build.Base.load_cached
    ~os_dir:Fpath.(scratch_cache / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
    | Some b -> b
    | None ->
      Printf.printf "Building base image...\n%!";
      Day11_opam_build.Base.build ~sw env ~cache_dir:scratch_cache
        ~os_distribution:"debian" ~os_version:"bookworm" ~arch:"x86_64"
        ~uid:(Unix.getuid ()) ~gid:(Unix.getgid ()) ()
      |> Result.get_ok
  in
  let os_dir = Fpath.(scratch_cache / "linux-x86_64") in
  let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir () in
  Day11_opam_build.Types.ensure_dirs benv;
  (* Get the store for loading packages at different commits *)
  let store, head_commit =
    Day11_opam.Git_utils.get_git_repo_store_and_hash opam_repository in
  let all_commits =
    let ic = Unix.open_process_in
      (Printf.sprintf "git -C %s log --format=%%H --since='%s' --until='%s'"
         opam_repository !since !until) in
    let commits = ref [] in
    (try while true do
       commits := (input_line ic) :: !commits
     done with End_of_file -> ());
    ignore (Unix.close_process_in ic);
    List.rev !commits
  in
  let sampled = all_commits in
  Printf.printf "Total commits: %d\n" (List.length all_commits);
  Printf.printf "Packages: %d\n\n%!" (List.length package_names);
  (* Track stats *)
  let total_solves = ref 0 in
  let total_reused = ref 0 in
  let total_builds = ref 0 in
  let total_cache_hits = ref 0 in
  let prev_solutions_dir = ref None in
  let prev_changed = ref OpamPackage.Name.Set.empty in
  (* Process each commit *)
  List.iteri (fun i commit_sha ->
    let short = String.sub commit_sha 0 12 in
    if (i + 1) mod 50 = 0 || i = 0 then
      Printf.printf "── Commit %d/%d: %s ──\n%!" (i + 1) (List.length sampled) short;
    let commit_hash =
      Day11_opam.Git_utils.resolve_commit_in_store store (Some commit_sha) in
    (* Load packages lazily *)
    let git_packages =
      Day11_opam.Git_packages.of_commit store commit_hash in
    (* Find latest versions *)
    let targets = List.filter_map (fun name ->
      find_latest_version git_packages name
    ) package_names in
    if (i + 1) mod 50 = 0 || i = 0 then
      Printf.printf "  targets: %d/%d found\n%!" (List.length targets) (List.length package_names);
    (* Compute changed packages from previous commit *)
    let solutions_dir = Fpath.(scratch_cache / "solutions" / short) in
    Bos.OS.Dir.create ~path:true solutions_dir |> ignore;
    let changed, reused =
      match !prev_solutions_dir with
      | None -> (OpamPackage.Name.Set.empty, 0)
      | Some prev_dir ->
        let changed =
          if i > 0 then
            let prev_sha = List.nth sampled (i - 1) in
            let prev_hash = Day11_opam.Git_utils.resolve_commit_in_store
              store (Some prev_sha) in
            let changed_names = Day11_opam.Git_packages.diff_packages
              ~store prev_hash commit_hash in
            List.fold_left (fun s n -> OpamPackage.Name.Set.add n s)
              OpamPackage.Name.Set.empty changed_names
          else OpamPackage.Name.Set.empty
        in
        let target_strs = List.map OpamPackage.to_string targets in
        let reused = Day11_batch.Incremental_solver.reuse_solutions
          ~solutions_cache_dir:solutions_dir ~previous_dir:prev_dir
          ~changed_packages:changed ~packages:target_strs in
        (changed, reused)
    in
    let n_changed = OpamPackage.Name.Set.cardinal changed in
    if n_changed > 0 || reused < List.length targets then
      Printf.printf "  [%d/%d] changed: %d, reused: %d, need solve: %d\n%!"
        (i + 1) (List.length sampled) n_changed reused
        (List.length targets - reused);
    prev_changed := changed;
    total_reused := !total_reused + reused;
    (* Solve remaining targets *)
    let need_solve = List.filter (fun target ->
      let cache_file = Fpath.(solutions_dir / (OpamPackage.to_string target ^ ".json")) in
      not (Sys.file_exists (Fpath.to_string cache_file))
    ) targets in
    let new_solutions = time (Printf.sprintf "solve %d packages" (List.length need_solve)) (fun () ->
      let results = Day11_solver_pool.Solver_pool.solve_many ~sw env
        ?ocaml_version ~np
        ~repos:[(opam_repository, commit_sha)] need_solve in
      List.filter_map (fun (target, result) ->
        match result with
        | Ok result ->
          let entry = Day11_batch.Incremental_solver.Cached_solution {
            package = target; result; cache_key = None } in
          ignore (Day11_batch.Incremental_solver.save
            Fpath.(solutions_dir / (OpamPackage.to_string target ^ ".json")) entry);
          Some (target, result.Day11_solution.Solve_result.build_deps)
        | Error (msg, _) ->
          Printf.printf "  SOLVE FAIL: %s: %s\n%!"
            (OpamPackage.to_string target)
            (String.sub msg 0 (min 80 (String.length msg)));
          None
      ) results) in
    total_solves := !total_solves + List.length need_solve;
    (* Load all solutions (reused + new) *)
    let all_solutions = List.filter_map (fun target ->
      let cache_file = Fpath.(solutions_dir / (OpamPackage.to_string target ^ ".json")) in
      match Day11_batch.Incremental_solver.load cache_file with
      | Ok (Cached_solution { result; _ }) -> Some (target, result.build_deps)
      | _ -> None
    ) targets in
    ignore new_solutions;
    if List.length need_solve > 0 then
      Printf.printf "  solutions: %d total (%d new)\n%!"
        (List.length all_solutions) (List.length new_solutions);
    (* Build DAG *)
    let find_opam = Day11_opam.Git_packages.find_package git_packages in
    let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
    let nodes = Day11_opam_build.Dag.build_dag cache ~base_hash:base.hash
      (List.map (fun (t, d) -> (t, d, d)) all_solutions) in
    if List.length need_solve > 0 then
      Printf.printf "  DAG: %d nodes\n%!" (List.length nodes);
    (* Build *)
    let built = ref 0 in
    let cached = ref 0 in
    let failed = ref 0 in
    time (Printf.sprintf "build %d nodes" (List.length nodes)) (fun () ->
      Day11_opam_build.Dag_executor.execute env ~np:4
        ~on_complete:(fun ~stats:_ ~cached:_ node success ->
          if success then begin
            let dir = Day11_opam_layer.Build.dir ~os_dir node in
            let layer_json = Fpath.(dir / "layer.json") in
            let is_cached = match Bos.OS.File.read layer_json with
              | Ok _ -> true | Error _ -> false in
            if is_cached then incr cached
            else incr built
          end else incr failed)
        ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> incr failed)
        nodes
        (fun node ->
          match Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] node () with
          | Day11_opam_build.Types.Success _ -> true
          | _ -> false));
    total_builds := !total_builds + !built;
    total_cache_hits := !total_cache_hits + !cached;
    if !built > 0 || !failed > 0 then
      Printf.printf "  built: %d new, %d cached, %d failed\n%!"
        !built !cached !failed;
    prev_solutions_dir := Some solutions_dir
  ) sampled;
  ignore (head_commit, prev_changed);
  Printf.printf "=== Summary ===\n";
  Printf.printf "  Commits processed: %d\n" (List.length sampled);
  Printf.printf "  Total solves: %d (reused: %d)\n" !total_solves !total_reused;
  Printf.printf "  Total builds: %d (cache hits: %d)\n" !total_builds !total_cache_hits;
  Printf.printf "Done.\n%!"
