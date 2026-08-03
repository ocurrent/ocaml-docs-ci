module Worker = Solver_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store
open Lwt.Infix

let env (vars : Worker.Vars.t) =
  let env =
    Opam_0install.Dir_context.std_env ~arch:vars.arch ~os:vars.os
      ~os_distribution:vars.os_distribution ~os_version:vars.os_version
      ~os_family:vars.os_family ()
  in
  function "opam-version" -> Some (OpamTypes.S "2.1.0") | v -> env v

let get_names = OpamFormula.fold_left (fun a (name, _) -> name :: a) []

let universes ?(post = false) ?(doc = false) ~packages
    (resolutions : OpamPackage.t list) =
  Printf.eprintf "DEBUG universes: post=%b doc=%b resolutions=%d\n%!" post doc
    (List.length resolutions);
  let aux root =
    let name, version = (OpamPackage.name root, OpamPackage.version root) in
    let opamfile : OpamFile.OPAM.t =
      try
        packages
        |> OpamPackage.Name.Map.find name
        |> OpamPackage.Version.Map.find version
      with Not_found ->
        Printf.eprintf "DEBUG: Package not found in packages map: %s\n%!"
          (OpamPackage.to_string root);
        raise Not_found
    in
    let deps =
      opamfile
      |> OpamFile.OPAM.depends
      |> OpamFilter.partial_filter_formula
           (OpamFilter.deps_var_env ~build:true ~post ~test:false ~doc
              ~dev_setup:false ~dev:false)
      |> get_names
      |> OpamPackage.Name.Set.of_list
    in
    let depopts =
      opamfile
      |> OpamFile.OPAM.depopts
      |> OpamFilter.partial_filter_formula
           (OpamFilter.deps_var_env ~build:true ~post ~test:false ~doc
              ~dev_setup:false ~dev:false)
      |> get_names
      |> OpamPackage.Name.Set.of_list
    in
    let all_deps = OpamPackage.Name.Set.union deps depopts in
    let deps =
      resolutions
      |> List.filter (fun res ->
          let name = OpamPackage.name res in
          OpamPackage.Name.Set.mem name all_deps)
    in
    let result = OpamPackage.Set.of_list deps in
    result
  in
  let simple_deps =
    List.fold_left
      (fun acc pkg -> OpamPackage.Map.add pkg (aux pkg) acc)
      OpamPackage.Map.empty resolutions
  in

  let rec closure pkgs =
    let deps =
      List.map
        (fun pkg -> OpamPackage.Map.find pkg simple_deps)
        (OpamPackage.Set.to_list pkgs)
    in
    let all = List.fold_left OpamPackage.Set.union OpamPackage.Set.empty deps in
    let new_deps = OpamPackage.Set.diff all pkgs in
    if OpamPackage.Set.is_empty new_deps then pkgs
    else closure (OpamPackage.Set.union pkgs all)
  in

  List.rev_map
    (fun pkg ->
      let name, version = (OpamPackage.name pkg, OpamPackage.version pkg) in
      let opamfile : OpamFile.OPAM.t =
        packages
        |> OpamPackage.Name.Map.find name
        |> OpamPackage.Version.Map.find version
      in
      let str = OpamFile.OPAM.write_to_string opamfile in
      ( pkg,
        str,
        closure (OpamPackage.Map.find pkg simple_deps)
        |> OpamPackage.Set.elements ))
    resolutions

type solve_result = Worker.solve_result = {
  compile_universes : (string * string * string list) list;
  link_universes : (string * string * string list) list;
}
[@@deriving yojson]

let solve ~packages ~constraints ~root_pkgs (vars : Worker.Vars.t) =
  let extended = Git_context.extend_packages packages in
  (* Single solve with doc:true to include all packages (including {with-doc} deps) *)
  let context =
    Git_context.create () ~packages:extended ~env:(env vars) ~constraints
      ~doc:true
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context root_pkgs in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n%!" (t1 -. t0);
  match r with
  | Ok sels ->
      let pkgs = Solver.packages_of_result sels in
      Printf.eprintf "DEBUG: Got %d packages from solve\n%!" (List.length pkgs);
      (* compile_universes: use original packages, exclude {with-doc} and {post} deps
         NOTE: ~doc:false is critical here because many packages have {with-doc} deps
         on odoc/documentation tools that create cycles (e.g., camlp-streams -> odoc -> odoc-parser -> camlp-streams) *)
      Printf.eprintf "DEBUG: Computing compile_universes...\n%!";
      let compile_universes = universes ~post:false ~doc:false ~packages pkgs in
      Printf.eprintf "DEBUG: compile_universes done (%d entries)\n%!"
        (List.length compile_universes);
      (* link_universes: use extended packages, include all deps *)
      Printf.eprintf "DEBUG: Computing link_universes...\n%!";
      let link_universes =
        universes ~post:true ~doc:true ~packages:extended pkgs
      in
      Printf.eprintf "DEBUG: link_universes done (%d entries)\n%!"
        (List.length link_universes);
      let map_universes univs =
        List.map
          (fun (pkg, str, univ) ->
            (OpamPackage.to_string pkg, str, List.map OpamPackage.to_string univ))
          univs
      in
      Ok
        {
          compile_universes = map_universes compile_universes;
          link_universes = map_universes link_universes;
        }
  | Error diagnostics -> Error (Solver.diagnostics diagnostics)

let test commit =
  Format.eprintf "Running test\n%!";
  let packages =
    Lwt_main.run
      ( Opam_repository.open_store () >>= fun store ->
        Git_context.read_packages store commit )
  in
  let root_pkgs =
    List.map OpamPackage.Name.of_string
      [ "ocaml"; "ocaml-base-compiler"; "vpnkit" ]
  in
  let constraints =
    List.map
      (fun (name, rel, version) ->
        ( OpamPackage.Name.of_string name,
          (rel, OpamPackage.Version.of_string version) ))
      [
        ("ocaml-base-compiler", `Geq, "4.08.0");
        ("ocaml", `Leq, "5.2.0");
        ("vpnkit", `Eq, "0.2.0");
      ]
    |> OpamPackage.Name.Map.of_list
  in
  let platform =
    {
      Worker.Vars.arch = "x86_64";
      os = "linux";
      os_distribution = "linux";
      os_family = "ubuntu";
      os_version = "20.04";
    }
  in
  Format.eprintf "Calling solve\n%!";
  match solve ~packages ~constraints ~root_pkgs platform with
  | Ok packages ->
      Printf.printf "%s\n"
        (solve_result_to_yojson packages |> Yojson.Safe.to_string)
  | Error msg -> Printf.printf "%s\n" msg

let main commit =
  let packages =
    Lwt_main.run
      ( Opam_repository.open_store () >>= fun store ->
        Git_context.read_packages store commit )
  in
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> ()
    | len ->
        let len = int_of_string len in
        let data = really_input_string stdin len in
        let request =
          Worker.Solve_request.of_yojson (Yojson.Safe.from_string data)
          |> Result.get_ok
        in
        let {
          Worker.Solve_request.opam_repository_commit;
          pkgs;
          constraints;
          platforms;
        } =
          request
        in
        let opam_repository_commit = Store.Hash.of_hex opam_repository_commit in
        assert (Store.Hash.equal opam_repository_commit commit);
        let root_pkgs = pkgs |> List.rev_map OpamPackage.Name.of_string in
        let constraints =
          constraints
          |> List.rev_map (fun (name, rel, version) ->
              ( OpamPackage.Name.of_string name,
                (rel, OpamPackage.Version.of_string version) ))
          |> OpamPackage.Name.Map.of_list
        in
        platforms
        |> List.iter (fun (_id, platform) ->
            let msg =
              match solve ~packages ~constraints ~root_pkgs platform with
              | Ok packages ->
                  "+"
                  ^ (solve_result_to_yojson packages |> Yojson.Safe.to_string)
              | Error msg -> "-" ^ msg
            in
            Printf.printf "%d\n%s%!" (String.length msg) msg);
        aux ()
  in
  aux ()

let main commit =
  try main commit
  with ex ->
    Fmt.epr "solver bug: %a@." Fmt.exn ex;
    let msg =
      match ex with Failure msg -> msg | ex -> Printexc.to_string ex
    in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex

(* Test with fake packages - no git needed *)
let test_fake () =
  (* Helper to create a simple opam with unfiltered depends *)
  let make_opam ?(depends = []) ?(doc_depends = []) ?(x_extra_doc_deps = []) ()
      =
    let empty = OpamFile.OPAM.empty in
    let mk_dep name =
      let name = OpamPackage.Name.of_string name in
      OpamFormula.Atom (name, OpamFormula.Empty)
    in
    (* Regular depends *)
    let depends_formula = List.map mk_dep depends |> OpamFormula.ands in
    (* Doc-filtered depends: name {with-doc} *)
    let doc_depends_formula =
      List.map
        (fun name ->
          let name = OpamPackage.Name.of_string name in
          let filter =
            OpamTypes.FIdent ([], OpamVariable.of_string "with-doc", None)
          in
          OpamFormula.Atom (name, OpamFormula.Atom (OpamTypes.Filter filter)))
        doc_depends
      |> OpamFormula.ands
    in
    let all_depends =
      OpamFormula.ands [ depends_formula; doc_depends_formula ]
    in
    let opam = OpamFile.OPAM.with_depends all_depends empty in
    (* Add x-extra-doc-deps extension if specified *)
    if x_extra_doc_deps = [] then opam
    else
      let ext_value =
        let deps_str =
          String.concat " & "
            (List.map (fun s -> "\"" ^ s ^ "\"") x_extra_doc_deps)
        in
        OpamParser.FullPos.value_from_string deps_str "<test>"
      in
      let extensions =
        OpamStd.String.Map.singleton "x-extra-doc-deps" ext_value
      in
      OpamFile.OPAM.with_extensions extensions opam
  in
  let make_packages pkgs =
    List.fold_left
      (fun acc (name, version, opam) ->
        let name = OpamPackage.Name.of_string name in
        let version = OpamPackage.Version.of_string version in
        let versions =
          match OpamPackage.Name.Map.find_opt name acc with
          | Some v -> v
          | None -> OpamPackage.Version.Map.empty
        in
        let versions = OpamPackage.Version.Map.add version opam versions in
        OpamPackage.Name.Map.add name versions acc)
      OpamPackage.Name.Map.empty pkgs
  in
  let test_env _ = None in
  (*
     Create fake packages to test both mechanisms:
     - mylib.1.0: depends on [base], has {doc} dep on [doc-helper], has x-extra-doc-deps [extra-helper]
     - base.1.0, base.2.0: no deps (test version pinning)
     - doc-helper.1.0: no deps (tests {doc} filtered deps)
     - extra-helper.1.0: no deps (tests x-extra-doc-deps extension)
  *)
  let packages =
    make_packages
      [
        ("base", "1.0", make_opam ());
        ("base", "2.0", make_opam ());
        ("doc-helper", "1.0", make_opam ());
        ("extra-helper", "1.0", make_opam ());
        ( "mylib",
          "1.0",
          make_opam ~depends:[ "base" ] ~doc_depends:[ "doc-helper" ]
            ~x_extra_doc_deps:[ "extra-helper" ] () );
      ]
  in
  let constraints = OpamPackage.Name.Map.empty in
  let root_pkgs = [ OpamPackage.Name.of_string "mylib" ] in

  Printf.printf "=== Testing two-phase solve with fake packages ===\n%!";
  Printf.printf
    "Testing both {with-doc} filtered deps AND x-extra-doc-deps extension\n%!";

  (* First solve: no doc deps *)
  let context =
    Git_context.create () ~packages ~env:test_env ~constraints ~doc:false
  in
  let result = Solver.solve context root_pkgs in
  match result with
  | Error e ->
      Printf.printf "First solve failed: %s\n" (Solver.diagnostics e);
      exit 1
  | Ok sels -> (
      let pkgs = Solver.packages_of_result sels in
      Printf.printf "\n[1] First solve (no with-doc deps):\n%!";
      List.iter (fun p -> Printf.printf "  %s\n" (OpamPackage.to_string p)) pkgs;

      let base_pkg =
        List.find (fun p -> OpamPackage.name_to_string p = "base") pkgs
      in
      Printf.printf "Base version selected: %s\n%!"
        (OpamPackage.version_to_string base_pkg);

      (* Verify doc-helper and extra-helper are NOT in first solve *)
      let has_doc_helper =
        List.exists (fun p -> OpamPackage.name_to_string p = "doc-helper") pkgs
      in
      let has_extra_helper =
        List.exists
          (fun p -> OpamPackage.name_to_string p = "extra-helper")
          pkgs
      in
      if has_doc_helper || has_extra_helper then (
        Printf.printf
          "FAILURE: First solve should not include doc/extra helpers\n%!";
        exit 1);

      (* Use extended packages (processes x-extra-doc-deps) *)
      let extended_packages = Git_context.extend_packages packages in
      (* Second solve: with doc deps, pinning first solve results *)
      (* IMPORTANT: pins must use extended_packages so x-extra-doc-deps are included *)
      let pins =
        List.fold_left
          (fun acc pkg ->
            let name = OpamPackage.name pkg in
            let version = OpamPackage.version pkg in
            let opam =
              OpamPackage.Name.Map.find name extended_packages
              |> OpamPackage.Version.Map.find version
            in
            OpamPackage.Name.Map.add name (version, opam) acc)
          OpamPackage.Name.Map.empty pkgs
      in
      let extended_context =
        Git_context.create () ~packages:extended_packages ~env:test_env
          ~constraints ~pins ~doc:true
      in
      let extended_result = Solver.solve extended_context root_pkgs in
      match extended_result with
      | Error e ->
          Printf.printf "Extended solve failed: %s\n%!" (Solver.diagnostics e);
          exit 1
      | Ok extended_sels ->
          let extended_pkgs = Solver.packages_of_result extended_sels in
          Printf.printf
            "\n[2] Extended solve (doc=true, post=true, with pins):\n%!";
          List.iter
            (fun p -> Printf.printf "  %s\n" (OpamPackage.to_string p))
            extended_pkgs;

          (* Verify base version is still the same (pinned) *)
          let extended_base_pkg =
            List.find
              (fun p -> OpamPackage.name_to_string p = "base")
              extended_pkgs
          in
          let base_version_preserved =
            OpamPackage.Version.equal
              (OpamPackage.version base_pkg)
              (OpamPackage.version extended_base_pkg)
          in

          let extra =
            List.filter (fun p -> not (List.mem p pkgs)) extended_pkgs
          in
          Printf.printf "\n[3] Extra packages from extended solve:\n%!";
          List.iter
            (fun p -> Printf.printf "  %s\n" (OpamPackage.to_string p))
            extra;

          let has_doc_helper =
            List.exists
              (fun p -> OpamPackage.name_to_string p = "doc-helper")
              extra
          in
          let has_extra_helper =
            List.exists
              (fun p -> OpamPackage.name_to_string p = "extra-helper")
              extra
          in

          Printf.printf "\n[4] Results:\n%!";
          Printf.printf "  Base version preserved (pinning works): %b\n%!"
            base_version_preserved;
          Printf.printf "  doc-helper added ({doc} filter works): %b\n%!"
            has_doc_helper;
          Printf.printf "  extra-helper added (x-extra-doc-deps works): %b\n%!"
            has_extra_helper;

          if base_version_preserved && has_doc_helper && has_extra_helper then (
            Printf.printf "\nSUCCESS: Both mechanisms work!\n%!";
            exit 0)
          else (
            Printf.printf "\nFAILURE: Something didn't work\n%!";
            exit 1))

(* Test with real opam-repository - tests x-extra-doc-deps on real packages *)
let test_real repo_path =
  Printf.printf "=== Testing with real opam-repository ===\n%!";
  Printf.printf "Repository: %s\n%!" repo_path;

  (* Get HEAD commit *)
  let commit_hex =
    let cmd = Printf.sprintf "git -C %s rev-parse HEAD" repo_path in
    let ic = Unix.open_process_in cmd in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim line
  in
  Printf.printf "Commit: %s\n%!" commit_hex;

  let commit = Store.Hash.of_hex commit_hex in
  let packages =
    Lwt_main.run
      ( Opam_repository.open_store_at repo_path >>= fun store ->
        Git_context.read_packages store commit )
  in
  Printf.printf "Loaded %d packages\n%!"
    (OpamPackage.Name.Map.cardinal packages);

  (* Test with odoc.3.1.0 which has x-extra-doc-deps *)
  let root_pkgs = List.map OpamPackage.Name.of_string [ "odoc" ] in
  let constraints =
    [ ("odoc", `Eq, "3.1.0"); ("ocaml", `Geq, "5.2.0") ]
    |> List.map (fun (name, rel, version) ->
        ( OpamPackage.Name.of_string name,
          (rel, OpamPackage.Version.of_string version) ))
    |> OpamPackage.Name.Map.of_list
  in
  let platform =
    {
      Worker.Vars.arch = "x86_64";
      os = "linux";
      os_distribution = "ubuntu";
      os_family = "debian";
      os_version = "22.04";
    }
  in

  Printf.printf "\nSolving for odoc.3.1.0...\n%!";
  match solve ~packages ~constraints ~root_pkgs platform with
  | Error msg ->
      Printf.printf "Solve failed: %s\n%!" msg;
      exit 1
  | Ok result ->
      Printf.printf "\nCompile universe for odoc.3.1.0:\n%!";
      let compile_pkgs =
        List.find_opt
          (fun (name, _, _) -> name = "odoc.3.1.0")
          result.compile_universes
      in
      let link_pkgs =
        List.find_opt
          (fun (name, _, _) -> name = "odoc.3.1.0")
          result.link_universes
      in
      (match compile_pkgs with
      | Some (_, _, deps) ->
          Printf.printf "  Compile deps (%d): %s\n%!" (List.length deps)
            (String.concat ", " (List.sort String.compare deps))
      | None -> Printf.printf "  odoc.3.1.0 not in compile_universes!\n%!");
      (match link_pkgs with
      | Some (_, _, deps) ->
          Printf.printf "  Link deps (%d): %s\n%!" (List.length deps)
            (String.concat ", " (List.sort String.compare deps))
      | None -> Printf.printf "  odoc.3.1.0 not in link_universes!\n%!");

      (* Check for x-extra-doc-deps packages: odoc-driver, sherlodoc, odig *)
      let expected_extra = [ "odoc-driver"; "sherlodoc"; "odig" ] in
      let link_deps =
        match link_pkgs with Some (_, _, deps) -> deps | None -> []
      in
      let compile_deps =
        match compile_pkgs with Some (_, _, deps) -> deps | None -> []
      in
      let extra_in_link =
        List.filter
          (fun name ->
            List.exists
              (fun dep ->
                String.sub dep 0 (min (String.length name) (String.length dep))
                = name)
              link_deps
            && not
                 (List.exists
                    (fun dep ->
                      String.sub dep 0
                        (min (String.length name) (String.length dep))
                      = name)
                    compile_deps))
          expected_extra
      in
      Printf.printf
        "\nExtra packages in link but not compile (from x-extra-doc-deps):\n%!";
      List.iter (fun p -> Printf.printf "  %s\n%!" p) extra_in_link;

      if List.length extra_in_link >= 2 then (
        Printf.printf
          "\nSUCCESS: x-extra-doc-deps packages found in link universe!\n%!";
        exit 0)
      else (
        Printf.printf
          "\nNote: Some x-extra-doc-deps packages may not be available.\n%!";
        Printf.printf
          "Check if odoc-driver, sherlodoc, odig exist in the repo.\n%!";
        exit 0)
