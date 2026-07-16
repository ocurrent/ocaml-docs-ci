(* Tests for the day11_solver library. *)

open Day11_solver
open Day11_test_util.Test_util

(* ── Helpers ─────────────────────────────────────────────────────── *)

let is_ok msg r = ok_or_fail msg r |> ignore

let pkg s = OpamPackage.of_string s

let _make_solution () =
  let c = pkg "c.1" and b = pkg "b.1" and a = pkg "a.1" in
  OpamPackage.Map.empty
  |> OpamPackage.Map.add c OpamPackage.Set.empty
  |> OpamPackage.Map.add b (OpamPackage.Set.singleton c)
  |> OpamPackage.Map.add a (OpamPackage.Set.of_list [ b; c ])

(* ── Opam_env tests ──────────────────────────────────────────────── *)

let test_std_env () =
  let env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  Alcotest.(check (option string)) "arch"
    (Some "x86_64")
    (env "arch" |> Option.map (function OpamTypes.S s -> s | _ -> "?"));
  Alcotest.(check (option string)) "os"
    (Some "linux")
    (env "os" |> Option.map (function OpamTypes.S s -> s | _ -> "?"))

let test_std_env_ocaml_native () =
  let env = Day11_opam.Opam_env.std_env
    ~ocaml_native:false
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  Alcotest.(check (option string)) "ocaml:native"
    (Some "false")
    (env "ocaml:native" |> Option.map (function
       OpamTypes.B b -> string_of_bool b | _ -> "?"))

let test_std_env_unknown () =
  let env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  Alcotest.(check bool) "unknown returns None"
    true (Option.is_none (env "foobar"))

(* ── Dot_solution tests ──────────────────────────────────────────── *)

let test_dot_to_string () =
  let s = _make_solution () in
  let dot = Dot_solution.to_string s in
  Alcotest.(check bool) "starts with digraph"
    true (Astring.String.is_prefix ~affix:"digraph" dot);
  Alcotest.(check bool) "contains a.1"
    true (Astring.String.is_infix ~affix:"a.1" dot)

let test_dot_save () = with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "graph.dot") in
  Dot_solution.save path (_make_solution ()) |> is_ok "save";
  Alcotest.(check bool) "file exists"
    true (Bos.OS.File.exists path |> Result.get_ok)

(* ── Solve tests (needs opam-repository git repo) ────────────────── *)

let test_solve_astring () =
  let opam_repo = opam_repository () in
  let packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repo in
    let env = Day11_opam.Opam_env.std_env
      ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
      ~os_family:"debian" ~os_version:"12" () in
    let result = Solve.solve ~packages ~env
      (pkg "astring.0.8.5") in
    match result with
    | Ok result ->
        let names = OpamPackage.Map.keys result.Day11_solution.Solve_result.build_deps
          |> List.map OpamPackage.to_string in
        Alcotest.(check bool) "has astring"
          true (List.exists (fun n ->
            Astring.String.is_prefix ~affix:"astring" n) names);
        Alcotest.(check bool) "has compiler"
          true (List.exists (fun n ->
            Astring.String.is_prefix ~affix:"ocaml-base-compiler" n
            || Astring.String.is_prefix ~affix:"ocaml-compiler" n) names)
    | Error (diag, _) ->
        Alcotest.fail (Printf.sprintf "Solve failed: %s" diag)

let test_solve_nonexistent () =
  let opam_repo = opam_repository () in
  let packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repo in
    let env = Day11_opam.Opam_env.std_env
      ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
      ~os_family:"debian" ~os_version:"12" () in
    let result = Solve.solve ~packages ~env
      (pkg "nonexistent-pkg-xyz.1.0") in
    Alcotest.(check bool) "no solution"
      true (Result.is_error result)

(* ── Two-graph doc deps tests ────────────────────────────────────── *)

(* Solve for odig, then check that odoc needs separate compile/link
   because its x-extra-doc-deps add deps in the link graph that are
   absent in the compile graph.

   odig depends on odoc, and odoc.3.1.0 has:
     x-extra-doc-deps: ["odoc-driver" "sherlodoc" "odig"]
   These appear in the solution as extra roots. The solver adds them
   as deps of odoc in the doc_deps graph, so odoc's deps differ
   between build_deps and doc_deps → needs separate link. *)
let test_odig_odoc_needs_separate_link () =
  let opam_repo = opam_repository () in
  let packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repo in
  let env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  let target = pkg "odig.0.0.9" in
  let result = Solve.solve ~packages ~env target in
  match result with
  | Error (diag, _) ->
    Alcotest.fail (Printf.sprintf "Solve failed: %s" diag)
  | Ok result ->
    (* odoc should be in the solution *)
    let odoc_pkg = OpamPackage.Map.fold (fun p _ acc ->
      if OpamPackage.Name.to_string (OpamPackage.name p) = "odoc"
      then Some p else acc
    ) result.Day11_solution.Solve_result.build_deps None in
    (match odoc_pkg with
     | None -> Alcotest.fail "odoc not in solution for odig"
     | Some odoc ->
       let separate = Day11_doc.Doc_deps.needs_separate_link result odoc in
       Alcotest.(check bool) "odoc needs separate link" true separate;
       (* odig itself should NOT need separate link
          (it has no x-extra-doc-deps or {post} deps) *)
       let odig_separate = Day11_doc.Doc_deps.needs_separate_link result target in
       Alcotest.(check bool) "odig single phase" false odig_separate)

(* doc_deps should be a superset of the build_deps for packages with
   x-extra-doc-deps, and identical deps for packages without.
   Verify this structurally for the odig solution. *)
let test_doc_deps_superset () =
  let opam_repo = opam_repository () in
  let packages, _store, _commit =
    Day11_opam.Git_packages.of_opam_repository opam_repo in
  let env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  let target = pkg "odig.0.0.9" in
  let result = Solve.solve ~packages ~env target in
  match result with
  | Error (diag, _) ->
    Alcotest.fail (Printf.sprintf "Solve failed: %s" diag)
  | Ok result ->
    let compile_deps = result.Day11_solution.Solve_result.build_deps in
    let link_deps = result.doc_deps in
    (* Same set of packages in both graphs *)
    let compile_pkgs = OpamPackage.Map.fold (fun p _ acc ->
      OpamPackage.Set.add p acc) compile_deps OpamPackage.Set.empty in
    let link_pkgs = OpamPackage.Map.fold (fun p _ acc ->
      OpamPackage.Set.add p acc) link_deps OpamPackage.Set.empty in
    Alcotest.(check bool) "same package set"
      true (OpamPackage.Set.equal compile_pkgs link_pkgs);
    (* For each package, link deps should be a superset of compile deps *)
    OpamPackage.Map.iter (fun p compile_set ->
      let link_set = OpamPackage.Map.find p link_deps in
      let missing = OpamPackage.Set.diff compile_set link_set in
      if not (OpamPackage.Set.is_empty missing) then
        Alcotest.fail (Printf.sprintf "%s: compile dep %s not in link deps"
          (OpamPackage.to_string p)
          (OpamPackage.to_string (OpamPackage.Set.choose missing)))
    ) compile_deps;
    (* odoc specifically should have extra link deps from x-extra-doc-deps *)
    let odoc_pkg = OpamPackage.Map.fold (fun p _ acc ->
      if OpamPackage.Name.to_string (OpamPackage.name p) = "odoc"
      then Some p else acc
    ) compile_deps None in
    (match odoc_pkg with
     | None -> Alcotest.fail "odoc not in solution"
     | Some odoc ->
       let compile_set = OpamPackage.Map.find odoc compile_deps in
       let link_set = OpamPackage.Map.find odoc link_deps in
       let extra = OpamPackage.Set.diff link_set compile_set in
       let extra_names = OpamPackage.Set.fold (fun p acc ->
         OpamPackage.Name.to_string (OpamPackage.name p) :: acc
       ) extra [] in
       Alcotest.(check bool) "odoc has extra link deps"
         true (List.length extra_names > 0);
       (* odig is in x-extra-doc-deps and not a regular dep of odoc,
          so it should appear as an extra link dep *)
       Alcotest.(check bool) "extra includes odig"
         true (List.mem "odig" extra_names))

(* ── Extras-vs-conflicts regression (pins only, no repo needed) ──── *)

(* Regression: x-extra-doc-deps must augment [depends:] only.
   Augmenting inside [Context.filter_deps] also rewrote the
   [conflicts:] formula (opam-0install routes both through
   filter_deps), turning each extra into "conflicts with all
   versions" and making any package with the field unsolvable —
   unless it had a real conflicts field providing an escape branch
   in the negated formula, which is why only some packages failed
   (eio.1.3 did, odig didn't). *)
let test_extras_not_conflicts () =
  let opam_of_string s = OpamFile.OPAM.read_from_string s in
  let pins =
    List.fold_left (fun acc (name, version, body) ->
      OpamPackage.Name.Map.add
        (OpamPackage.Name.of_string name)
        (OpamPackage.Version.of_string version, opam_of_string body)
        acc)
      OpamPackage.Name.Map.empty
      [ ("ocaml-base-compiler", "5.0.0", {|opam-version: "2.0"|});
        ("extra1", "1", {|opam-version: "2.0"|});
        ("extra2", "1", {|opam-version: "2.0"|});
        (* No conflicts: field — the shape that used to fail. *)
        ("tgt", "1", {|
opam-version: "2.0"
x-extra-doc-deps: [ "extra1" {= version} "extra2" ]
|}) ]
  in
  let env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in
  match Solve.solve ~packages:Day11_opam.Git_packages.empty
          ~env ~pins ~doc:true (pkg "tgt.1") with
  | Error (diag, _) ->
    Alcotest.fail (Printf.sprintf "Solve failed: %s" diag)
  | Ok result ->
    let solved =
      OpamPackage.Set.fold (fun p acc ->
        OpamPackage.Name.to_string (OpamPackage.name p) :: acc)
        result.Day11_solution.Solve_result.packages []
    in
    List.iter (fun n ->
      Alcotest.(check bool) (n ^ " in solution")
        true (List.mem n solved))
      [ "tgt"; "extra1"; "extra2" ];
    (* Extras are doc deps of the target, not build deps. *)
    let target = OpamPackage.of_string "tgt.1" in
    let doc_set = OpamPackage.Map.find target result.doc_deps in
    let build_set = OpamPackage.Map.find target result.build_deps in
    let mem name set =
      OpamPackage.Set.exists (fun p ->
        OpamPackage.Name.to_string (OpamPackage.name p) = name) set in
    Alcotest.(check bool) "extra1 in doc deps" true (mem "extra1" doc_set);
    Alcotest.(check bool) "extra2 in doc deps" true (mem "extra2" doc_set);
    Alcotest.(check bool) "extra1 not in build deps"
      false (mem "extra1" build_set)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_solver"
    [
      ( "Opam_env",
        [
          Alcotest.test_case "std_env" `Quick test_std_env;
          Alcotest.test_case "ocaml_native" `Quick test_std_env_ocaml_native;
          Alcotest.test_case "unknown var" `Quick test_std_env_unknown;
        ] );
      ( "Dot_solution",
        [
          Alcotest.test_case "to_string" `Quick test_dot_to_string;
          Alcotest.test_case "save" `Quick test_dot_save;
        ] );
( "Solve",
        [
          Alcotest.test_case "extras not conflicts" `Quick
            test_extras_not_conflicts;
          Alcotest.test_case "solve astring" `Slow test_solve_astring;
          Alcotest.test_case "solve nonexistent" `Slow test_solve_nonexistent;
          Alcotest.test_case "odig: odoc needs separate link" `Slow
            test_odig_odoc_needs_separate_link;
          Alcotest.test_case "doc_deps superset" `Slow
            test_doc_deps_superset;
        ] );
    ]
