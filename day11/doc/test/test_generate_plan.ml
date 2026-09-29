(* Pure (container-free) tests for the doc-DAG planner core,
   [Generate.build_internal_plan]. Runs on any platform — no overlayfs,
   no runc.

   The regression under test: a package that appears in several solutions
   with the same build-deps closure but different doc-deps closures
   ("universes"). [odoc.3.2.1] in the real pipeline is such a package
   (injected as an x-extra-doc-dep into ~everything). Blessing picks the
   richest universe; the doc nodes must include a node carrying exactly
   that universe, and it must be the blessed one — otherwise nothing is
   blessed and no HTML renders. *)

module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
module Deps = Day11_solution.Deps
module Universe = Day11_solution.Universe

let p name ver =
  OpamPackage.create
    (OpamPackage.Name.of_string name)
    (OpamPackage.Version.of_string ver)

(* Build a Deps.t (pkg -> direct deps) from an assoc list. *)
let deps_of (entries : (OpamPackage.t * OpamPackage.t list) list) : Deps.t =
  List.fold_left
    (fun acc (pkg, ds) ->
      OpamPackage.Map.add pkg (OpamPackage.Set.of_list ds) acc)
    OpamPackage.Map.empty entries

let solve_result ~build_deps ~doc_deps : Day11_solution.Solve_result.t =
  let packages =
    OpamPackage.Map.fold
      (fun k _ acc -> OpamPackage.Set.add k acc)
      doc_deps OpamPackage.Set.empty
  in
  { packages; build_deps; doc_deps; examined = OpamPackage.Name.Set.empty }

(* A minimal Tool.t whose single build's hash equals the tool hash —
   that's what [find_odoc_final_for_hash]/[driver_final] look for. *)
let mk_tool ~hash ~pkg : Tool.t =
  {
    hash;
    dir = Fpath.v ("/tmp/tool-" ^ hash);
    builds = [ { Build.hash; pkg; deps = []; universe = Universe.dummy } ];
  }

(* ── Packages ─────────────────────────────────────────────────── *)
let ocaml = p "ocaml" "5.4.1" (* virtual — marks documentable *)
let comp = p "ocaml-base-compiler" "5.4.1" (* solver compiler *)
let pp = p "pkgp" "1.0" (* the multi-universe package *)
let extradoc = p "extradoc" "1.0" (* doc-only extra dep, B only *)
let appa = p "appa" "1.0"
let appb = p "appb" "1.0"
let odoc_pkg = p "odoc" "3.0.0"
let driver_pkg = p "odoc-driver" "3.0.0"

(* Solution A: appA -> pkgp; pkgp's doc_deps == build_deps. *)
let sol_a =
  let bd =
    deps_of
      [
        (ocaml, []);
        (comp, []);
        (pp, [ ocaml; comp ]);
        (appa, [ pp; ocaml; comp ]);
      ]
  in
  solve_result ~build_deps:bd ~doc_deps:bd

(* Solution B: appB -> pkgp; pkgp keeps the SAME build_deps but gains an
   x-extra-doc-dep [extradoc] in its doc_deps -> a distinct, richer
   universe for pkgp sharing pkgp's build hash. *)
let sol_b =
  let bd =
    deps_of
      [
        (ocaml, []);
        (comp, []);
        (extradoc, [ ocaml; comp ]);
        (pp, [ ocaml; comp ]);
        (appb, [ pp; ocaml; comp ]);
      ]
  in
  let dd =
    deps_of
      [
        (ocaml, []);
        (comp, []);
        (extradoc, [ ocaml; comp ]);
        (pp, [ ocaml; comp; extradoc ]);
        (appb, [ pp; ocaml; comp; extradoc ]);
      ]
  in
  solve_result ~build_deps:bd ~doc_deps:dd

let solutions = [ (appa, sol_a); (appb, sol_b) ]

(* [extra] packages ride along in the tool's closure, like the ~90 deps
   of a real odoc-driver build. *)
let mk_tool_with ~hash ~pkg ~extra : Tool.t =
  let b pkg h = { Build.hash = h; pkg; deps = []; universe = Universe.dummy } in
  {
    hash;
    dir = Fpath.v ("/tmp/tool-" ^ hash);
    builds =
      b pkg hash
      :: List.mapi (fun i x -> b x (Printf.sprintf "%s-%d" hash i)) extra;
  }

let build_plan_with ~driver_tool ~odoc_tools =
  let cache =
    Day11_opam_build.Hash_cache.create ~find_opam:(fun _ -> None) ()
  in
  let triples =
    List.map
      (fun (t, (r : Day11_solution.Solve_result.t)) ->
        (t, r.build_deps, r.doc_deps))
      solutions
  in
  let nodes =
    Day11_opam_build.Dag.build_dag cache ~base_hash:"test-base" triples
  in
  Day11_doc.Generate.build_internal_plan ~os_dir:(Fpath.v "/tmp/os") ~cache
    ~base_hash:"test-base" ~driver_tool ~odoc_tools ~nodes ~solutions

let build_plan () =
  let cache =
    Day11_opam_build.Hash_cache.create ~find_opam:(fun _ -> None) ()
  in
  let triples =
    List.map
      (fun (t, (r : Day11_solution.Solve_result.t)) ->
        (t, r.build_deps, r.doc_deps))
      solutions
  in
  let nodes =
    Day11_opam_build.Dag.build_dag cache ~base_hash:"test-base" triples
  in
  let driver_tool = mk_tool ~hash:"driverhash00" ~pkg:driver_pkg in
  let odoc_tools = [ (comp, mk_tool ~hash:"odochash0000" ~pkg:odoc_pkg) ] in
  Day11_doc.Generate.build_internal_plan ~os_dir:(Fpath.v "/tmp/os") ~cache
    ~base_hash:"test-base" ~driver_tool ~odoc_tools ~nodes ~solutions

(* The richer universe pkgp should be blessed in: of_deps of its
   transitive doc_deps in solution B = {ocaml, comp, extradoc}. *)
let pp_blessed_universe =
  Universe.to_string
    (Universe.of_deps (OpamPackage.Set.of_list [ ocaml; comp; extradoc ]))

let primary_kind = function
  | Day11_doc.Generate.Compile | Doc_all -> true
  | _ -> false

let pp_doc_nodes plan =
  Hashtbl.fold
    (fun _ (dn : Day11_doc.Generate.doc_node) acc ->
      if OpamPackage.equal dn.build_node.pkg pp && primary_kind dn.kind then
        dn :: acc
      else acc)
    plan.Day11_doc.Generate.meta []

let test_pkgp_has_two_universes () =
  let plan = build_plan () in
  let universes =
    List.sort_uniq compare
      (List.map
         (fun (dn : Day11_doc.Generate.doc_node) -> dn.universe)
         (pp_doc_nodes plan))
  in
  Alcotest.(check bool)
    "pkgp has >= 2 distinct compile/doc-all universes" true
    (List.length universes >= 2)

let test_pkgp_blessed_universe () =
  let plan = build_plan () in
  let blessed =
    List.filter
      (fun (dn : Day11_doc.Generate.doc_node) -> dn.blessed)
      (pp_doc_nodes plan)
  in
  Alcotest.(check int)
    "exactly one blessed compile/doc-all node for pkgp" 1 (List.length blessed);
  Alcotest.(check string)
    "the blessed node carries the richest (extradoc) universe"
    pp_blessed_universe (List.hd blessed).universe

let test_universes_have_distinct_layer_hashes () =
  let plan = build_plan () in
  let hashes =
    List.map
      (fun (dn : Day11_doc.Generate.doc_node) -> dn.layer.hash)
      (pp_doc_nodes plan)
  in
  Alcotest.(check int)
    "one distinct layer hash per pkgp doc node" (List.length hashes)
    (List.length (List.sort_uniq compare hashes))

(* Doc and link layer hashes of every doc node in a plan, sorted. *)
let doc_hashes plan =
  Hashtbl.fold
    (fun _ (dn : Day11_doc.Generate.doc_node) acc ->
      match dn.kind with
      | Compile | Doc_all | Link -> dn.layer.hash :: acc
      | Build | Tool -> acc)
    plan.Day11_doc.Generate.meta []
  |> List.sort compare

let plan_with ?(odoc = odoc_pkg) ?(driver_hash = "driverhash00")
    ?(odoc_hash = "odochash0000") ?(driver_extra = [ p "mtime" "2.1.0" ]) () =
  build_plan_with
    ~driver_tool:
      (mk_tool_with ~hash:driver_hash ~pkg:driver_pkg ~extra:driver_extra)
    ~odoc_tools:[ (comp, mk_tool_with ~hash:odoc_hash ~pkg:odoc ~extra:[]) ]

(* The churn this guards against: a dep deep in a tool's closure bumps,
   so the tool's layer hash moves while no output-shaping version does.
   Doc layers must keep their hashes, or every doc in the plan rebuilds. *)
let test_tool_rebuild_keeps_doc_hashes () =
  let before = doc_hashes (plan_with ()) in
  let after =
    doc_hashes
      (plan_with ~driver_hash:"driverhash99" ~odoc_hash:"odochash9999"
         ~driver_extra:[ p "mtime" "2.2.0" ]
         ())
  in
  Alcotest.(check bool) "plan has doc nodes" true (before <> []);
  Alcotest.(check (list string)) "doc hashes unchanged" before after

let test_odoc_bump_changes_doc_hashes () =
  let before = doc_hashes (plan_with ()) in
  let after = doc_hashes (plan_with ~odoc:(p "odoc" "3.0.1") ()) in
  List.iter
    (fun h ->
      Alcotest.(check bool) "every doc hash moved" false (List.mem h before))
    after

let test_output_dep_bump_changes_doc_hashes () =
  let before =
    doc_hashes (plan_with ~driver_extra:[ p "cmarkit" "0.3.0" ] ())
  in
  let after = doc_hashes (plan_with ~driver_extra:[ p "cmarkit" "0.4.0" ] ()) in
  Alcotest.(check bool) "cmarkit bump rehashes docs" true (before <> after)

let () =
  Alcotest.run "generate_plan"
    [
      ( "per-universe doc nodes",
        [
          Alcotest.test_case "pkgp has two universes" `Quick
            test_pkgp_has_two_universes;
          Alcotest.test_case "pkgp blessed in richest universe" `Quick
            test_pkgp_blessed_universe;
          Alcotest.test_case "distinct layer hash per universe" `Quick
            test_universes_have_distinct_layer_hashes;
        ] );
      ( "doc tool identity",
        [
          Alcotest.test_case "tool rebuild keeps doc hashes" `Quick
            test_tool_rebuild_keeps_doc_hashes;
          Alcotest.test_case "odoc bump changes doc hashes" `Quick
            test_odoc_bump_changes_doc_hashes;
          Alcotest.test_case "output dep bump changes doc hashes" `Quick
            test_output_dep_bump_changes_doc_hashes;
        ] );
    ]
