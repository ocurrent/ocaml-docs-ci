let build_per_compiler ~sw env benv ~np ~packages ~repos ~mounts
    ~extra_repo_dirs ~repo_dir ~solutions =
  Printf.printf "\nBuilding JTW tools from %s...\n%!" repo_dir;
  let compiler_versions =
    let seen = Hashtbl.create 4 in
    List.filter_map
      (fun (_target, solution) ->
        match Day11_doc.Generate.find_compiler solution with
        | Some c when not (Hashtbl.mem seen (OpamPackage.to_string c)) ->
            Hashtbl.replace seen (OpamPackage.to_string c) ();
            Some c
        | _ -> None)
      solutions
  in
  List.filter_map
    (fun compiler_v ->
      Printf.printf "Building JTW for %s...\n%!"
        (OpamPackage.to_string compiler_v);
      match
        Day11_opam_build.Tools.build_tool_from_repo ~sw env benv ~np ~packages
          ~repos ~ocaml_version:compiler_v ~mounts ~extra_repo_dirs ~repo_dir
          ~extra_target_names:Tool_layer.extra_tool_targets
          ~target_name:Tool_layer.tool_target ()
      with
      | Ok tool ->
          Printf.printf "JTW tools for %s: OK\n%!"
            (OpamPackage.to_string compiler_v);
          Some (compiler_v, tool)
      | Error (`Msg e) ->
          Printf.printf "JTW tools for %s failed: %s\n%!"
            (OpamPackage.to_string compiler_v)
            e;
          None)
    compiler_versions

let build_and_run ~sw env benv ~np ~os_dir ~packages ~repos ~mounts
    ~extra_repo_dirs ~repo_dir ~output ~nodes ~solutions =
  let jtw_tools =
    build_per_compiler ~sw env benv ~np ~packages ~repos ~mounts
      ~extra_repo_dirs ~repo_dir ~solutions
  in
  if jtw_tools = [] then
    Printf.printf "No JTW tools built, skipping generation\n%!"
  else
    let jtw_results, worker_layers =
      Generate.run ~sw env benv ~os_dir ~jtw_tools ~nodes ~solutions
    in
    Generate.assemble ~os_dir ~output ~jtw_results ~worker_layers ~solutions;
    Printf.printf "\n=== JTW: %d packages, %d workers ===\n%!"
      (Hashtbl.length jtw_results)
      (List.length worker_layers)
