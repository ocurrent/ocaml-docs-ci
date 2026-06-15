(* Tests for the day11_solution library. *)

open Day11_solution
open Day11_test_util.Test_util

let is_ok msg r = ok_or_fail msg r |> ignore

let is_error _msg = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error, got Ok"

let pkg s = OpamPackage.of_string s

let make_solution () =
  let c = pkg "c.1" and b = pkg "b.1" and a = pkg "a.1" in
  OpamPackage.Map.empty
  |> OpamPackage.Map.add c OpamPackage.Set.empty
  |> OpamPackage.Map.add b (OpamPackage.Set.singleton c)
  |> OpamPackage.Map.add a (OpamPackage.Set.of_list [ b; c ])

(* ── Graph tests ─────────────────────────────────────────────────── *)

let test_transitive_deps () =
  let s = make_solution () in
  let t = Deps.transitive_deps s in
  let a_deps = OpamPackage.Map.find (pkg "a.1") t in
  Alcotest.(check bool) "a has b"
    true (OpamPackage.Set.mem (pkg "b.1") a_deps);
  Alcotest.(check bool) "a has c"
    true (OpamPackage.Set.mem (pkg "c.1") a_deps);
  let c_deps = OpamPackage.Map.find (pkg "c.1") t in
  Alcotest.(check bool) "c empty"
    true (OpamPackage.Set.is_empty c_deps)

(* Cyclic graph: a -> b -> c -> a, and c -> x (x a leaf). Every cycle
   member can reach the whole cycle plus x. The old order-dependent
   closure truncated at the back-edge: starting the DFS at [a], [c] came
   out as {a, x} — missing [b], which c reaches via c -> a -> b. The SCC
   closure must give every member the complete, identical set. *)
let test_transitive_deps_cycle () =
  let a = pkg "a.1" and b = pkg "b.1" and c = pkg "c.1" and x = pkg "x.1" in
  let g =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add a (OpamPackage.Set.singleton b)
    |> OpamPackage.Map.add b (OpamPackage.Set.singleton c)
    |> OpamPackage.Map.add c (OpamPackage.Set.of_list [ a; x ])
    |> OpamPackage.Map.add x OpamPackage.Set.empty
  in
  let t = Deps.transitive_deps g in
  let clo p = OpamPackage.Map.find p t in
  let mem who p = OpamPackage.Set.mem p (clo who) in
  (* completeness — the case the old code got wrong *)
  Alcotest.(check bool) "c reaches b (regression)" true (mem c b);
  Alcotest.(check bool) "c reaches x" true (mem c x);
  Alcotest.(check bool) "a reaches x" true (mem a x);
  (* cycle members are mutually reachable -> identical closures *)
  Alcotest.(check bool) "a = b = c closure" true
    (OpamPackage.Set.equal (clo a) (clo b)
     && OpamPackage.Set.equal (clo b) (clo c));
  Alcotest.(check bool) "cycle closure = {a,b,c,x}" true
    (OpamPackage.Set.equal (clo a)
       (OpamPackage.Set.of_list [ a; b; c; x ]));
  (* leaf x reaches nothing *)
  Alcotest.(check bool) "x empty" true (OpamPackage.Set.is_empty (clo x))

let test_extract_ocaml_version () =
  let s =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.5.1.0") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "dune.3.0") OpamPackage.Set.empty
  in
  let v = Deps.extract_ocaml_version s in
  Alcotest.(check (option string)) "found compiler"
    (Some "ocaml-base-compiler.5.1.0")
    (Option.map OpamPackage.to_string v)

let test_extract_ocaml_version_none () =
  let s = OpamPackage.Map.singleton (pkg "dune.3.0") OpamPackage.Set.empty in
  let v = Deps.extract_ocaml_version s in
  Alcotest.(check bool) "no compiler" true (Option.is_none v)

(* ── Rdeps tests ────────────────────────────────────────────────── *)

let test_rdeps_transitive () =
  (* a -> b -> c (a only directly depends on b, not c) *)
  let s =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
    |> OpamPackage.Map.add (pkg "a.1")
         (OpamPackage.Set.singleton (pkg "b.1"))
  in
  let rdeps = Rdeps.find [ s ] (pkg "c.1") in
  (* both a and b transitively depend on c *)
  Alcotest.(check bool) "b is rdep of c"
    true (OpamPackage.Set.mem (pkg "b.1") rdeps);
  Alcotest.(check bool) "a is transitive rdep of c"
    true (OpamPackage.Set.mem (pkg "a.1") rdeps);
  (* c is not an rdep of itself *)
  Alcotest.(check bool) "c not rdep of itself"
    false (OpamPackage.Set.mem (pkg "c.1") rdeps)

let test_rdeps_not_found () =
  let s = make_solution () in
  let rdeps = Rdeps.find [ s ] (pkg "nonexistent.1") in
  Alcotest.(check bool) "empty"
    true (OpamPackage.Set.is_empty rdeps)

let test_rdeps_multiple () =
  (* s1: a -> b -> c, s2: d -> c *)
  let s1 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
    |> OpamPackage.Map.add (pkg "a.1")
         (OpamPackage.Set.singleton (pkg "b.1"))
  in
  let s2 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "d.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let rdeps = Rdeps.find [ s1; s2 ] (pkg "c.1") in
  Alcotest.(check bool) "a from s1" true (OpamPackage.Set.mem (pkg "a.1") rdeps);
  Alcotest.(check bool) "b from s1" true (OpamPackage.Set.mem (pkg "b.1") rdeps);
  Alcotest.(check bool) "d from s2" true (OpamPackage.Set.mem (pkg "d.1") rdeps)

(* ── Solution_json tests ─────────────────────────────────────────── *)

let make_json_solution () =
  let dune = pkg "dune.3.0" in
  let yojson = pkg "yojson.2.2.2" in
  OpamPackage.Map.empty
  |> OpamPackage.Map.add dune OpamPackage.Set.empty
  |> OpamPackage.Map.add yojson (OpamPackage.Set.of_list [ dune ])

let test_solution_json_roundtrip () =
  let s = make_json_solution () in
  let json = Json.to_json s in
  let s2 = Json.of_json json |> ok_or_fail "of_json" in
  Alcotest.(check bool) "roundtrip"
    true (OpamPackage.Map.equal OpamPackage.Set.equal s s2)

let test_solution_string_roundtrip () =
  let s = make_json_solution () in
  let str = Json.to_string s in
  let s2 = Json.of_string str |> ok_or_fail "of_string" in
  Alcotest.(check bool) "roundtrip"
    true (OpamPackage.Map.equal OpamPackage.Set.equal s s2)

let test_solution_file_roundtrip () = with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "solution.json") in
  let s = make_json_solution () in
  Json.save path s |> is_ok "save";
  let s2 = Json.load path |> ok_or_fail "load" in
  Alcotest.(check bool) "roundtrip"
    true (OpamPackage.Map.equal OpamPackage.Set.equal s s2)

let test_solution_corrupt () =
  is_error "corrupt" (Json.of_json (`String "not an object"));
  is_error "corrupt" (Json.of_string "{broken")

let test_solution_load_missing () =
  is_error "missing" (Json.load (Fpath.v "/nonexistent/solution.json"))

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_solution"
    [
      ( "Deps",
        [
          Alcotest.test_case "transitive_deps" `Quick test_transitive_deps;
          Alcotest.test_case "transitive_deps cycle" `Quick test_transitive_deps_cycle;
Alcotest.test_case "extract_ocaml_version" `Quick
            test_extract_ocaml_version;
          Alcotest.test_case "extract_ocaml_version none" `Quick
            test_extract_ocaml_version_none;
        ] );
      ( "Json",
        [
          Alcotest.test_case "json roundtrip" `Quick test_solution_json_roundtrip;
          Alcotest.test_case "string roundtrip" `Quick test_solution_string_roundtrip;
          Alcotest.test_case "file roundtrip" `Quick test_solution_file_roundtrip;
          Alcotest.test_case "corrupt input" `Quick test_solution_corrupt;
          Alcotest.test_case "load missing" `Quick test_solution_load_missing;
        ] );
      ( "Rdeps",
        [
          Alcotest.test_case "transitive" `Quick test_rdeps_transitive;
          Alcotest.test_case "not found" `Quick test_rdeps_not_found;
          Alcotest.test_case "multiple solutions" `Quick test_rdeps_multiple;
        ] );
    ]
