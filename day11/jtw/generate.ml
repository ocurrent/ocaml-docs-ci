module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool

type build = Build.t

let generate_package ~sw env benv ~os_dir ~(jtw_tool : Tool.t) (node : build) =
  let pkg_dir = Build.dir ~os_dir node in
  let installed_libs =
    Day11_opam_layer.Installed_files.scan_libs ~layer_dir:pkg_dir
  in
  if installed_libs = [] then None
  else
    let findlib_names = Gen.findlib_names_of_installed_libs installed_libs in
    if findlib_names = [] then None
    else
      (* JTW needs the full tool layer stacked (not just bind-mounted binaries)
       because jtw uses ocamlfind at runtime to discover libraries *)
      let cmd =
        "eval $(opam env) && "
        ^ Gen.container_script ~pkg:node.pkg ~installed_libs
      in
      let hash =
        Gen.compute_layer_hash ~build_hash:node.hash ~tools_hash:jtw_tool.hash
      in
      let jtw_node : build =
        {
          hash;
          pkg = node.pkg;
          deps = jtw_tool.builds @ [ node ];
          universe = Day11_solution.Universe.dummy;
        }
      in
      match
        Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[]
          jtw_node
          ~strategy:{ cmd; cleanup = (fun ~sw:_ _ _ -> ()) }
          ()
      with
      | Day11_opam_build.Types.Success bl ->
          Printf.printf "  %s: OK\n%!" (OpamPackage.to_string node.pkg);
          Some bl
      | _ ->
          Printf.printf "  %s: FAILED\n%!" (OpamPackage.to_string node.pkg);
          None

(** Build worker.js for a solution. Stacks all build layers from the solution
    and runs [jtw opam -o ... stdlib]. *)
let build_worker ~sw env benv ~(jtw_tool : Tool.t) ~solution_nodes =
  let cmd =
    "eval $(opam env) && " ^ "jtw opam -o /home/opam/jtw-worker-output stdlib"
  in
  let dep_hashes = List.map (fun (n : build) -> n.hash) solution_nodes in
  let hash =
    Day11_layer.Hash.of_strings ([ "jtw-worker"; jtw_tool.hash ] @ dep_hashes)
  in
  let dummy_pkg = OpamPackage.of_string "jtw-worker.0" in
  let worker_node : build =
    {
      hash;
      pkg = dummy_pkg;
      deps = jtw_tool.builds @ solution_nodes;
      universe = Day11_solution.Universe.dummy;
    }
  in
  match
    Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[]
      worker_node
      ~strategy:{ cmd; cleanup = (fun ~sw:_ _ _ -> ()) }
      ()
  with
  | Day11_opam_build.Types.Success bl ->
      Printf.printf "  worker.js: OK\n%!";
      Some bl
  | _ ->
      Printf.printf "  worker.js: FAILED\n%!";
      None

let run ~sw env benv ~os_dir ~jtw_tools ~nodes ~solutions =
  (* Map each (pkg, universe) build_hash to its compiler. Keying by
     [pkg] alone would collapse universes: the same (name, version)
     can build against different compilers in different solutions. *)
  let bh_compiler = Hashtbl.create 64 in
  List.iter
    (fun (_target, solution) ->
      match Day11_doc.Generate.find_compiler solution with
      | None -> ()
      | Some compiler ->
          let trans = Day11_solution.Deps.transitive_deps solution in
          OpamPackage.Map.iter
            (fun pkg deps ->
              let u_s =
                Day11_solution.Universe.to_string
                  (Day11_solution.Universe.of_deps deps)
              in
              Hashtbl.replace bh_compiler
                (OpamPackage.to_string pkg, u_s)
                compiler)
            trans)
    solutions;
  let find_jtw_tool (node : build) =
    let u_s = Day11_solution.Universe.to_string node.universe in
    match
      Hashtbl.find_opt bh_compiler (OpamPackage.to_string node.pkg, u_s)
    with
    | None -> None
    | Some compiler ->
        List.find_opt (fun (c, _) -> OpamPackage.equal c compiler) jtw_tools
        |> Option.map snd
  in
  (* Per-package generation *)
  Printf.printf "  JTW per-package (%d packages)...\n%!" (List.length nodes);
  let jtw_results : (OpamPackage.t, build) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun (node : build) ->
      match find_jtw_tool node with
      | None -> ()
      | Some jtw_tool -> (
          match generate_package ~sw env benv ~os_dir ~jtw_tool node with
          | Some bl -> Hashtbl.replace jtw_results node.pkg bl
          | None -> ()))
    nodes;
  Printf.printf "  JTW: %d packages processed\n%!" (Hashtbl.length jtw_results);
  (* Worker.js per solution *)
  Printf.printf "  JTW worker.js...\n%!";
  let worker_layers =
    List.filter_map
      (fun (_target, solution) ->
        match Day11_doc.Generate.find_compiler solution with
        | None -> None
        | Some compiler -> (
            match
              List.find_opt
                (fun (c, _) -> OpamPackage.equal c compiler)
                jtw_tools
            with
            | None -> None
            | Some (_, jtw_tool) -> (
                let solution_nodes =
                  List.filter
                    (fun (node : build) ->
                      OpamPackage.Map.mem node.pkg solution)
                    nodes
                in
                match build_worker ~sw env benv ~jtw_tool ~solution_nodes with
                | Some bl -> Some (compiler, bl)
                | None -> None)))
      solutions
  in
  (jtw_results, worker_layers)

(** Assemble JTW output from per-package layers and worker layer.

    Output structure:
    {v
      <output>/
        compiler/<ocaml-version>/<hash>/
          worker.js
          lib/ocaml/ (stdlib .cmi files)
        p/<package>/<version>/<hash>/
          lib/<findlib-name>/
            META, dynamic_cmis.json, *.cmi, *.cma.js
        u/<universe-hash>/
          findlib_index.json
    v} *)
let assemble ~os_dir ~output ~jtw_results ~worker_layers ~solutions =
  Bos.OS.Dir.create ~path:true (Fpath.v output) |> ignore;
  (* Compiler/worker output *)
  List.iter
    (fun (compiler_v, worker_bl) ->
      let ocaml_ver =
        OpamPackage.Version.to_string (OpamPackage.version compiler_v)
      in
      let worker_dir =
        Fpath.(
          Build.dir ~os_dir worker_bl
          / "fs"
          / "home"
          / "opam"
          / "jtw-worker-output")
      in
      let worker_dir_s = Fpath.to_string worker_dir in
      if Sys.file_exists worker_dir_s then
        let compiler_hash = Gen.compute_compiler_content_hash worker_dir_s in
        let dst = Fpath.(v output / "compiler" / ocaml_ver / compiler_hash) in
        if not (Bos.OS.Dir.exists dst |> Result.get_ok) then (
          Bos.OS.Dir.create ~path:true dst |> ignore;
          let worker_js = Fpath.(worker_dir / "worker.js") in
          if Bos.OS.File.exists worker_js |> Result.get_ok then
            ignore
              (Bos.OS.Cmd.run
                 Bos.Cmd.(
                   v "cp"
                   % Fpath.to_string worker_js
                   % Fpath.to_string Fpath.(dst / "worker.js")));
          let lib_src = Fpath.(worker_dir / "lib") in
          if Bos.OS.Dir.exists lib_src |> Result.get_ok then
            ignore
              (Bos.OS.Cmd.run
                 Bos.Cmd.(
                   v "cp"
                   % "-a"
                   % Fpath.to_string lib_src
                   % Fpath.to_string Fpath.(dst / "lib")))))
    worker_layers;
  (* Per-package output *)
  Hashtbl.iter
    (fun pkg bl ->
      let pkg_name = OpamPackage.name_to_string pkg in
      let pkg_version = OpamPackage.version_to_string pkg in
      let jtw_output =
        Fpath.(
          Build.dir ~os_dir bl
          / "fs"
          / "home"
          / "opam"
          / "jtw-output"
          / pkg_name
          / "lib")
      in
      let jtw_output_s = Fpath.to_string jtw_output in
      if Sys.file_exists jtw_output_s then
        let content_hash = Gen.compute_content_hash jtw_output_s in
        let dst =
          Fpath.(v output / "p" / pkg_name / pkg_version / content_hash / "lib")
        in
        if not (Bos.OS.Dir.exists dst |> Result.get_ok) then (
          Bos.OS.Dir.create ~path:true dst |> ignore;
          ignore
            (Bos.OS.Cmd.run
               Bos.Cmd.(
                 v "cp"
                 % "-a"
                 % "--no-target-directory"
                 % jtw_output_s
                 % Fpath.to_string dst))))
    jtw_results;
  (* Universe findlib indexes *)
  List.iter
    (fun (_target, solution) ->
      let build_hashes =
        OpamPackage.Map.fold
          (fun _pkg _deps acc ->
            (* We'd need the build hash from the node — for now use package string *)
            acc)
          solution []
      in
      let universe =
        Day11_layer.Hash.of_strings (List.sort String.compare build_hashes)
      in
      let u_dir = Fpath.(v output / "u" / universe) in
      Bos.OS.Dir.create ~path:true u_dir |> ignore)
    solutions;
  Printf.printf "  JTW output assembled in %s\n%!" output
