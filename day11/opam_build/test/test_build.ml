(* Tests for the day11_opam_build library. *)

open Day11_opam_build

(* ── Helpers ─────────────────────────────────────────────────────── *)

let pkg s = OpamPackage.of_string s

(* ── Types tests ─────────────────────────────────────────────────── *)

let test_build_result_variants () =
  let _s =
    Types.Success
      {
        hash = "build-abc";
        pkg = pkg "x.1";
        deps = [];
        universe = Day11_solution.Universe.dummy;
      }
  in
  let _f = Types.Failure "build-abc123" in
  let _d = Types.Dependency_failed in
  let _n = Types.No_solution "unsatisfiable" in
  let _sol = Types.Solution OpamPackage.Map.empty in
  ()

let test_build_node () =
  let node : Day11_opam_layer.Build.t =
    {
      hash = "build-abc123";
      pkg = pkg "yojson.2.2.2";
      deps =
        [
          {
            hash = "build-def456";
            pkg = pkg "dune.3.0";
            deps = [];
            universe = Day11_solution.Universe.dummy;
          };
        ];
      universe = Day11_solution.Universe.dummy;
    }
  in
  Alcotest.(check string) "pkg" "yojson.2.2.2" (OpamPackage.to_string node.pkg);
  Alcotest.(check string) "hash" "build-abc123" node.hash

(* ── Hash_cache tests ────────────────────────────────────────────── *)

let make_find_opam opam_str pkg =
  let opam = OpamFile.OPAM.read_from_string opam_str in
  let opam = OpamFile.OPAM.with_name (OpamPackage.name pkg) opam in
  let opam = OpamFile.OPAM.with_version (OpamPackage.version pkg) opam in
  Some opam

let test_hash_cache_pkg () =
  let opam_str = {|opam-version: "2.0"
depends: ["ocaml"]|} in
  let find_opam p = make_find_opam opam_str p in
  let cache = Hash_cache.create ~find_opam () in
  let h1 = Hash_cache.pkg_opam_hash cache (pkg "astring.0.8.5") in
  let h2 = Hash_cache.pkg_opam_hash cache (pkg "astring.0.8.5") in
  Alcotest.(check string) "deterministic" h1 h2;
  Alcotest.(check bool) "non-empty" true (String.length h1 > 0);
  (* Different opam content → different hash *)
  let find_opam2 p =
    make_find_opam {|opam-version: "2.0"
depends: ["ocaml" "fmt"]|} p
  in
  let cache2 = Hash_cache.create ~find_opam:find_opam2 () in
  let h3 = Hash_cache.pkg_opam_hash cache2 (pkg "astring.0.8.5") in
  Alcotest.(check bool) "varies with content" true (h1 <> h3)

let test_hash_cache_layer () =
  let find_opam p = make_find_opam {|opam-version: "2.0"|} p in
  let cache = Hash_cache.create ~find_opam () in
  let h1 =
    Hash_cache.layer_hash cache ~base_hash:"base1" [ pkg "astring.0.8.5" ]
  in
  let h2 =
    Hash_cache.layer_hash cache ~base_hash:"base1" [ pkg "astring.0.8.5" ]
  in
  Alcotest.(check string) "deterministic" h1 h2;
  let h3 =
    Hash_cache.layer_hash cache ~base_hash:"base2" [ pkg "astring.0.8.5" ]
  in
  Alcotest.(check bool) "varies with base" true (h1 <> h3)

(* Two distinct packages whose version dirs are byte-identical in
   opam-repository share a version-dir tree OID — really happens:
   js_of_ocaml-ppx / js_of_ocaml-ppx_deriving_json since 6.1.0
   (templated opam files built with the [name] variable),
   hidapi.1.0-1 / hidapi.1.0, … The global digest cache must not serve
   one twin's digest for the other: the parse stamps name/version into
   the effective part, so their digests — and hence their layer
   hashes — differ even though the dir content is identical. A
   predecessor keyed by bare OID collapsed twin layer hashes, and
   [Dag.build_dag]'s memo-by-hash then dropped one twin from the plan,
   so its dependents built without it (eliom.12.1.0 without
   ppx_deriving_json). *)
let test_hash_cache_twin_dirs () =
  let opam_str = {|opam-version: "2.0"
depends: ["ocaml"]|} in
  let find_opam p = make_find_opam opam_str p in
  let find_oid _pkg = Some "aabbccdd" in
  (* shared tree OID *)
  let cache = Hash_cache.create ~find_opam ~find_oid () in
  let twin_a = pkg "js_of_ocaml-ppx.6.4.1" in
  let twin_b = pkg "js_of_ocaml-ppx_deriving_json.6.4.1" in
  let ha = Hash_cache.pkg_opam_hash cache twin_a in
  let hb = Hash_cache.pkg_opam_hash cache twin_b in
  Alcotest.(check bool) "twin dirs get distinct digests" true (ha <> hb);
  let la = Hash_cache.layer_hash cache ~base_hash:"b" [ twin_a ] in
  let lb = Hash_cache.layer_hash cache ~base_hash:"b" [ twin_b ] in
  Alcotest.(check bool) "twin layer hashes differ" true (la <> lb);
  (* The global cache outlives cache instances (it's what makes
     Profile_ctx reloads cheap) and must keep the twins distinct. *)
  let cache2 = Hash_cache.create ~find_opam ~find_oid () in
  Alcotest.(check string)
    "twin a digest stable across instances" ha
    (Hash_cache.pkg_opam_hash cache2 twin_a);
  Alcotest.(check string)
    "twin b digest stable across instances" hb
    (Hash_cache.pkg_opam_hash cache2 twin_b)

(* The global digest cache is validated by tree OID: a fresh cache
   instance must serve cached digests without re-parsing while the
   package's OID is unchanged, and re-parse when it moves. *)
let test_hash_cache_oid_validator () =
  let parses = ref 0 in
  let find_opam_counting opam_str p =
    incr parses;
    make_find_opam opam_str p
  in
  let p1 = pkg "hashcache-validator-test.1.0" in
  let v1 = {|opam-version: "2.0"
depends: ["ocaml"]|} in
  let oid = ref "oid-1" in
  let find_oid _ = Some !oid in
  let cache =
    Hash_cache.create ~find_opam:(find_opam_counting v1) ~find_oid ()
  in
  let h1 = Hash_cache.pkg_opam_hash cache p1 in
  Alcotest.(check int) "first lookup parses" 1 !parses;
  (* New instance, same OID: served from the global cache, no parse. *)
  let cache2 =
    Hash_cache.create ~find_opam:(find_opam_counting v1) ~find_oid ()
  in
  let h2 = Hash_cache.pkg_opam_hash cache2 p1 in
  Alcotest.(check int) "unchanged OID served without parse" 1 !parses;
  Alcotest.(check string) "same digest" h1 h2;
  (* OID moved (content changed): must re-parse and see the new digest. *)
  oid := "oid-2";
  let v2 = {|opam-version: "2.0"
depends: ["ocaml" "fmt"]|} in
  let cache3 =
    Hash_cache.create ~find_opam:(find_opam_counting v2) ~find_oid ()
  in
  let h3 = Hash_cache.pkg_opam_hash cache3 p1 in
  Alcotest.(check int) "moved OID re-parses" 2 !parses;
  Alcotest.(check bool) "digest follows content" true (h1 <> h3)

(* ── Base tests ──────────────────────────────────────────────────── *)

let test_base_hash_deterministic () =
  let h1 = Base.hash ~image:"ocaml/opam:debian-ocaml-5.2" in
  let h2 = Base.hash ~image:"ocaml/opam:debian-ocaml-5.2" in
  Alcotest.(check string) "deterministic" h1 h2

let test_base_hash_varies () =
  let h1 = Base.hash ~image:"ocaml/opam:debian-ocaml-5.2" in
  let h2 = Base.hash ~image:"ocaml/opam:debian-ocaml-5.1" in
  Alcotest.(check bool) "different" true (h1 <> h2)

(* ── Dag tests ───────────────────────────────────────────────────── *)

let test_dag_empty () =
  let find_opam _p = None in
  let cache = Hash_cache.create ~find_opam () in
  let nodes = Dag.build_dag cache ~base_hash:"base" [] in
  Alcotest.(check int) "empty" 0 (List.length nodes)

let test_dag_single_solution () =
  let find_opam p = make_find_opam {|opam-version: "2.0"|} p in
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1") (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let cache = Hash_cache.create ~find_opam () in
  let nodes =
    Dag.build_dag cache ~base_hash:"base" [ (pkg "b.1", solution, solution) ]
  in
  Alcotest.(check int) "2 nodes" 2 (List.length nodes);
  let names =
    List.map
      (fun (n : Day11_opam_layer.Build.t) -> OpamPackage.to_string n.pkg)
      nodes
  in
  Alcotest.(check (list string)) "topo order" [ "c.1"; "b.1" ] names

let test_dag_dedup_across_solutions () =
  (* c.1 appears in both solutions with the same deps — should be
     deduplicated to one node *)
  let find_opam p = make_find_opam {|opam-version: "2.0"|} p in
  let sol1 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "a.1") (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let sol2 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1") (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let cache = Hash_cache.create ~find_opam () in
  let nodes =
    Dag.build_dag cache ~base_hash:"base"
      [ (pkg "a.1", sol1, sol1); (pkg "b.1", sol2, sol2) ]
  in
  (* c.1, a.1, b.1 — c.1 appears once despite being in 2 solutions *)
  Alcotest.(check int) "3 nodes (c deduplicated)" 3 (List.length nodes);
  let c_nodes =
    List.filter
      (fun (n : Day11_opam_layer.Build.t) ->
        OpamPackage.to_string n.pkg = "c.1")
      nodes
  in
  Alcotest.(check int) "1 c node" 1 (List.length c_nodes)

let test_dag_different_universes () =
  (* c.1 in two solutions with different deps — NOT deduplicated *)
  let find_opam p = make_find_opam {|opam-version: "2.0"|} p in
  let sol1 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "d.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "c.1") (OpamPackage.Set.singleton (pkg "d.1"))
  in
  let sol2 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "e.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "c.1") (OpamPackage.Set.singleton (pkg "e.1"))
  in
  let cache = Hash_cache.create ~find_opam () in
  let nodes =
    Dag.build_dag cache ~base_hash:"base"
      [ (pkg "c.1", sol1, sol1); (pkg "c.1", sol2, sol2) ]
  in
  let c_nodes =
    List.filter
      (fun (n : Day11_opam_layer.Build.t) ->
        OpamPackage.to_string n.pkg = "c.1")
      nodes
  in
  (* c.1 with dep d.1 vs c.1 with dep e.1 — different universes *)
  Alcotest.(check int) "2 c nodes (different universes)" 2 (List.length c_nodes);
  (* Universes should differ *)
  let u1 = (List.nth c_nodes 0).universe in
  let u2 = (List.nth c_nodes 1).universe in
  Alcotest.(check bool)
    "different universes" false
    (Day11_solution.Universe.equal u1 u2)

let test_dag_universe_set () =
  (* Check universe is computed from transitive deps *)
  let find_opam p = make_find_opam {|opam-version: "2.0"|} p in
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "d.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "c.1") (OpamPackage.Set.singleton (pkg "d.1"))
    |> OpamPackage.Map.add (pkg "b.1") (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let cache = Hash_cache.create ~find_opam () in
  let nodes =
    Dag.build_dag cache ~base_hash:"base" [ (pkg "b.1", solution, solution) ]
  in
  let b_node =
    List.find
      (fun (n : Day11_opam_layer.Build.t) ->
        OpamPackage.to_string n.pkg = "b.1")
      nodes
  in
  (* b.1's universe should include c.1 and d.1 (transitive deps) *)
  let expected =
    Day11_solution.Universe.of_deps
      (OpamPackage.Set.of_list [ pkg "c.1"; pkg "d.1" ])
  in
  Alcotest.(check bool)
    "universe includes transitive deps" true
    (Day11_solution.Universe.equal b_node.universe expected)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_opam_build"
    [
      ( "Types",
        [
          Alcotest.test_case "build_result variants" `Quick
            test_build_result_variants;
          Alcotest.test_case "build_node" `Quick test_build_node;
        ] );
      ( "Hash_cache",
        [
          Alcotest.test_case "pkg_opam_hash" `Quick test_hash_cache_pkg;
          Alcotest.test_case "layer_hash" `Quick test_hash_cache_layer;
          Alcotest.test_case "twin dirs" `Quick test_hash_cache_twin_dirs;
          Alcotest.test_case "oid validator" `Quick
            test_hash_cache_oid_validator;
        ] );
      ( "Base",
        [
          Alcotest.test_case "hash deterministic" `Quick
            test_base_hash_deterministic;
          Alcotest.test_case "hash varies" `Quick test_base_hash_varies;
          Alcotest.test_case "build_hash deterministic" `Quick (fun () ->
              let h1 =
                Base.build_hash ~os_distribution:"debian" ~os_version:"bookworm"
                  ~arch:"x86_64" ()
              in
              let h2 =
                Base.build_hash ~os_distribution:"debian" ~os_version:"bookworm"
                  ~arch:"x86_64" ()
              in
              Alcotest.(check string) "same" h1 h2);
          Alcotest.test_case "build_hash varies" `Quick (fun () ->
              let h1 =
                Base.build_hash ~os_distribution:"debian" ~os_version:"bookworm"
                  ~arch:"x86_64" ()
              in
              let h2 =
                Base.build_hash ~os_distribution:"ubuntu" ~os_version:"24.04"
                  ~arch:"x86_64" ()
              in
              Alcotest.(check bool) "different" true (h1 <> h2));
        ] );
      ( "Dag",
        [
          Alcotest.test_case "empty" `Quick test_dag_empty;
          Alcotest.test_case "single solution" `Quick test_dag_single_solution;
          Alcotest.test_case "dedup across solutions" `Quick
            test_dag_dedup_across_solutions;
          Alcotest.test_case "different universes" `Quick
            test_dag_different_universes;
          Alcotest.test_case "universe set" `Quick test_dag_universe_set;
        ] );
    ]
