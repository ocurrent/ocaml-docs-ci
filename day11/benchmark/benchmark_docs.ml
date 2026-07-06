(* Benchmark: doc generation for astring with proper solve + bind-mounted tools *)

let opam_repository =
  try Sys.getenv "OPAM_REPOSITORY"
  with Not_found -> "/home/jjl25/opam-repository"

let scratch_cache = Fpath.v "/tmp/day11-scratch-cache"

let time name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "%-45s %.3fs\n%!" name elapsed;
  result

let () =
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Printf.printf "=== Doc generation benchmark ===\n\n";
  Eio_main.run @@ fun env -> Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let base = match Day11_opam_build.Base.load_cached
    ~os_dir:Fpath.(scratch_cache / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
    | Some b -> b
    | None -> Printf.printf "No cache\n%!"; exit 1
  in
  let os_dir = Fpath.(scratch_cache / "linux-x86_64") in
  let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir
    ~uid:1000 ~gid:1000 () in
  Day11_opam_build.Types.ensure_dirs benv;
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories
      [ (opam_repository, None) ] in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  (* Build odoc-driver tools *)
  let odoc_tool = time "Build odoc-driver tools (cache)" (fun () ->
    Day11_opam_build.Tools.build_tool ~sw env benv
      ~packages:git_packages ~repos:repos_with_shas
      (OpamPackage.of_string "odoc-driver.3.1.0")
    |> Result.get_ok) in
  let tool_mounts, odoc_bin, odoc_md_bin =
    Day11_doc.Tool_binaries.doc_tool_mounts odoc_tool in
  let voodoo_bin = "/home/opam/doc-tools/bin/odoc_driver_voodoo" in
  (* Solve astring properly *)
  let astring_pkg = OpamPackage.of_string "astring.0.8.5" in
  let astring_result = time "Solve astring" (fun () ->
    Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
      astring_pkg |> Result.get_ok) in
  let astring_solution = astring_result.Day11_solution.Solve_result.build_deps in
  (* Build astring with real deps *)
  let astring_nodes = Day11_opam_build.Dag.build_dag cache ~base_hash:base.hash
    [ (astring_pkg, astring_solution, astring_solution) ] in
  let astring_build = time "Build astring + deps (cache)" (fun () ->
    Day11_opam_build.Dag_executor.execute env ~np:4
      ~on_complete:(fun ~stats:_ ~cached:_ _ _ -> ())
      ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
      astring_nodes
      (fun node ->
        match Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] node () with
        | Day11_opam_build.Types.Success _ -> true | _ -> false);
    List.find (fun (n : Day11_opam_layer.Build.t) ->
      OpamPackage.equal n.pkg astring_pkg) astring_nodes) in
  let pkg_dir = Day11_opam_layer.Build.dir ~os_dir astring_build in
  Printf.printf "  astring: %d real deps\n%!"
    (List.length astring_build.deps);
  (* Prep *)
  let installed_libs = Day11_opam_layer.Installed_files.scan_libs ~layer_dir:pkg_dir in
  let installed_docs = Day11_opam_layer.Installed_files.scan_docs ~layer_dir:pkg_dir in
  let universe = Day11_doc.Command.compute_universe_hash
    (List.map (fun (b : Day11_opam_layer.Build.t) -> b.hash)
       astring_nodes) in
  Printf.printf "\n--- Single-phase (--actions all) ---\n%!";
  (* Delete existing doc layer *)
  let doc_hash = Day11_layer.Hash.of_strings
    [ "doc-all"; astring_build.hash; odoc_tool.hash; universe ] in
  let doc_dir = Day11_layer.Layer.dir
    (Day11_layer.Layer.of_hash ~os_dir doc_hash) in
  ignore (Day11_sys.Sudo.rm_rf ~sw env doc_dir);
  let prep_dir = Bos.OS.Dir.tmp "day11_bench_%s" |> Result.get_ok in
  ignore (Day11_doc.Prep.create_with_mounts ~source_layer_dir:pkg_dir
    ~dest_layer_dir:prep_dir ~universe ~pkg:astring_pkg
    ~installed_libs ~installed_docs);
  let prep_mount = Day11_container.Mount.bind_ro
    ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
    "/home/opam/prep" in
  let all_mounts = prep_mount :: tool_mounts in
  let cmd = Printf.sprintf
    "eval $(opam env) && %s %s \
     --odoc-dir /home/opam/odoc-out \
     --html-dir /home/opam/html \
     --actions all -j $(nproc) -v --blessed \
     --odoc %s --odoc-md %s"
    voodoo_bin "astring" odoc_bin odoc_md_bin in
  let doc_node : Day11_opam_layer.Build.t =
    { hash = doc_hash; pkg = astring_pkg;
      deps = astring_build.deps @ [ astring_build ]; universe = Day11_solution.Universe.dummy } in
  let html_count = time "Doc gen astring (cold, single phase)" (fun () ->
    match Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:all_mounts
            doc_node ~strategy:{ cmd; cleanup = fun ~sw:_ _ _ -> () } () with
    | Day11_opam_build.Types.Success bl ->
      let dd = Day11_opam_layer.Build.dir ~os_dir bl in
      let find_run = Day11_sys.Run.run ~sw env
        Bos.Cmd.(v "find" % Fpath.to_string Fpath.(dd / "fs")
                 % "-name" % "*.html" % "-type" % "f") None in
      List.length (String.split_on_char '\n' (String.trim find_run.output)
        |> List.filter (fun s -> s <> ""))
    | _ -> 0) in
  Printf.printf "  → %d HTML files\n%!" html_count;
  (* Cache hit *)
  ignore (time "Doc gen astring (cache hit)" (fun () ->
    Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] ~mounts:all_mounts
      doc_node ~strategy:{ cmd; cleanup = fun ~sw:_ _ _ -> () } ()));
  ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir);
  Printf.printf "\nDone.\n%!"
