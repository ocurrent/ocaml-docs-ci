(* Tests for day11_batch library. *)

open Day11_batch
open Day11_opam_build
open Day11_test_util.Test_util

let pkg s = OpamPackage.of_string s

(* ── Blessing tests ──────────────────────────────────────────────── *)

let test_blessing_single_universe () =
  (* One target, one solution — every package blessed *)
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let blessings = Blessing.compute_blessings
    [ (pkg "b.1", solution) ] in
  match blessings with
  | [ (_, map) ] ->
      Alcotest.(check bool) "b blessed"
        true (Blessing.is_blessed map (pkg "b.1"));
      Alcotest.(check bool) "c blessed"
        true (Blessing.is_blessed map (pkg "c.1"))
  | _ -> Alcotest.fail "expected 1 blessing map"

let test_blessing_two_universes () =
  (* Two targets sharing package c — c should be blessed in the
     richer universe (more deps) *)
  let sol1 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "a.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let sol2 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "d.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1")
         (OpamPackage.Set.of_list [ pkg "c.1"; pkg "d.1" ])
  in
  let blessings = Blessing.compute_blessings
    [ (pkg "a.1", sol1); (pkg "b.1", sol2) ] in
  Alcotest.(check int) "2 maps" 2 (List.length blessings);
  (* c.1 should be blessed in sol2 (richer: 3 packages vs 2) *)
  let _, map2 = List.nth blessings 1 in
  Alcotest.(check bool) "c blessed in richer"
    true (Blessing.is_blessed map2 (pkg "c.1"))

let test_blessing_empty () =
  let blessings = Blessing.compute_blessings [] in
  Alcotest.(check int) "empty" 0 (List.length blessings)

let test_blessing_compiler_tiebreaker () =
  (* Two solutions with same deps_count for c.1 but different compilers.
     Should prefer the newer compiler. *)
  let sol_old =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.4.14.0")
         OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "c.1")
         (OpamPackage.Set.singleton (pkg "ocaml-base-compiler.4.14.0"))
  in
  let sol_new =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.5.4.1")
         OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "c.1")
         (OpamPackage.Set.singleton (pkg "ocaml-base-compiler.5.4.1"))
  in
  let blessings = Blessing.compute_blessings
    [ (pkg "a.1", sol_old); (pkg "b.1", sol_new) ] in
  (* c.1 has 1 dep in both — tiebreaker should pick 5.4.1 *)
  let _, map_new = List.nth blessings 1 in
  Alcotest.(check bool) "c blessed in newer compiler solution"
    true (Blessing.is_blessed map_new (pkg "c.1"))

let test_blessed_universes () =
  let sol =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "c.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "b.1")
         (OpamPackage.Set.singleton (pkg "c.1"))
  in
  let blessed = Blessing.compute_blessed_universes
    [ (pkg "b.1", sol) ] in
  (* Both packages should have blessed universes *)
  Alcotest.(check bool) "b has blessed universe"
    true (Hashtbl.mem blessed (pkg "b.1"));
  Alcotest.(check bool) "c has blessed universe"
    true (Hashtbl.mem blessed (pkg "c.1"))

(* ── Dag_executor tests ──────────────────────────────────────────── *)

let test_dag_executor_basic () = with_eio @@ fun ~sw:_ env ->
  let completed = ref [] in
  let node_c : Day11_opam_layer.Build.t =
    { hash = "build-c"; pkg = pkg "c.1"; deps = []; universe = Day11_solution.Universe.dummy } in
  let node_b : Day11_opam_layer.Build.t =
    { hash = "build-b"; pkg = pkg "b.1"; deps = [node_c]; universe = Day11_solution.Universe.dummy } in
  let nodes = [ node_c; node_b ] in
  Dag_executor.execute env ~np:2
    ~on_complete:(fun ~stats:_ ~cached:_ node success ->
      completed := (OpamPackage.to_string node.pkg, success) :: !completed)
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
    nodes
    (fun _node -> true);  (* all succeed *)
  let completed = List.rev !completed in
  Alcotest.(check int) "2 completed" 2 (List.length completed);
  (* c must complete before b *)
  Alcotest.(check string) "first" "c.1" (fst (List.hd completed))

let test_dag_executor_failure_cascade () = with_eio @@ fun ~sw:_ env ->
  let cascaded = ref [] in
  let node_c : Day11_opam_layer.Build.t =
    { hash = "build-c"; pkg = pkg "c.1"; deps = []; universe = Day11_solution.Universe.dummy } in
  let node_b : Day11_opam_layer.Build.t =
    { hash = "build-b"; pkg = pkg "b.1"; deps = [node_c]; universe = Day11_solution.Universe.dummy } in
  let nodes = [ node_c; node_b ] in
  Dag_executor.execute env ~np:2
    ~on_complete:(fun ~stats:_ ~cached:_ _node _success -> ())
    ~on_cascade:(fun ~failed ~failed_dep:_ ->
      cascaded := failed.hash :: !cascaded)
    nodes
    (fun node ->
      (* c fails *)
      OpamPackage.to_string node.pkg <> "c.1");
  Alcotest.(check int) "1 cascade" 1 (List.length !cascaded);
  Alcotest.(check string) "b cascaded" "build-b" (List.hd !cascaded)

(* ── Incremental_solver tests ───────────────────────────────────── *)

let test_save_load_solution () =
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "pkg.json") in
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml.5.2.0") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "astring.0.8.5")
         (OpamPackage.Set.singleton (pkg "ocaml.5.2.0")) in
  let entry = Incremental_solver.Cached_solution {
    package = pkg "astring.0.8.5";
    result = { Day11_solution.Solve_result.
      packages = OpamPackage.Map.fold (fun p _ acc -> OpamPackage.Set.add p acc)
        solution OpamPackage.Set.empty;
      build_deps = solution;
      doc_deps = solution;
      examined =
        OpamPackage.Name.Set.of_list
          (List.map OpamPackage.Name.of_string [ "astring"; "ocaml"; "dune" ]);
    };
    cache_key = None;
  } in
  (match Incremental_solver.save path entry with
   | Ok () -> ()
   | Error (`Msg e) -> Alcotest.fail e);
  match Incremental_solver.load path with
  | Error (`Msg e) -> Alcotest.fail e
  | Ok (Cached_failure _) -> Alcotest.fail "expected solution"
  | Ok (Cached_solution s) ->
    Alcotest.(check string) "package"
      "astring.0.8.5" (OpamPackage.to_string s.package);
    Alcotest.(check int) "solution size" 2
      (OpamPackage.Map.cardinal s.result.build_deps);
    Alcotest.(check int) "examined size" 3
      (OpamPackage.Name.Set.cardinal s.result.examined)

let test_save_load_failure () =
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "bad.json") in
  let entry = Incremental_solver.Cached_failure {
    package = pkg "broken.1.0";
    error = "no solution";
    examined =
      OpamPackage.Name.Set.singleton
        (OpamPackage.Name.of_string "broken");
    cache_key = None;
  } in
  (match Incremental_solver.save path entry with
   | Ok () -> ()
   | Error (`Msg e) -> Alcotest.fail e);
  match Incremental_solver.load path with
  | Error (`Msg e) -> Alcotest.fail e
  | Ok (Cached_solution _) -> Alcotest.fail "expected failure"
  | Ok (Cached_failure f) ->
    Alcotest.(check string) "package"
      "broken.1.0" (OpamPackage.to_string f.package);
    Alcotest.(check string) "error" "no solution" f.error

let test_reuse_no_overlap () =
  (* Examined set {astring, ocaml} doesn't overlap changed {dune} → reuse *)
  with_tmp_dir @@ fun dir ->
  let prev_dir = Fpath.(dir / "prev") in
  let cur_dir = Fpath.(dir / "cur") in
  ignore (Bos.OS.Dir.create prev_dir);
  ignore (Bos.OS.Dir.create cur_dir);
  let solution = OpamPackage.Map.singleton (pkg "astring.0.8.5") OpamPackage.Set.empty in
  let entry = Incremental_solver.Cached_solution {
    package = pkg "astring.0.8.5";
    result = { Day11_solution.Solve_result.
      packages = OpamPackage.Set.singleton (pkg "astring.0.8.5");
      build_deps = solution;
      doc_deps = solution;
      examined =
        OpamPackage.Name.Set.of_list
          (List.map OpamPackage.Name.of_string [ "astring"; "ocaml" ]);
    };
    cache_key = None;
  } in
  (match Incremental_solver.save Fpath.(prev_dir / "astring.0.8.5.json") entry with
   | Ok () -> () | Error (`Msg e) -> Alcotest.fail e);
  let changed = OpamPackage.Name.Set.singleton
    (OpamPackage.Name.of_string "dune") in
  let reused = Incremental_solver.reuse_solutions
    ~solutions_cache_dir:cur_dir ~previous_dir:prev_dir
    ~changed_packages:changed ~packages:["astring.0.8.5"] () in
  Alcotest.(check int) "1 reused" 1 reused;
  (* Verify the file was hardlinked *)
  match Incremental_solver.load Fpath.(cur_dir / "astring.0.8.5.json") with
  | Ok (Cached_solution _) -> ()
  | _ -> Alcotest.fail "expected reused solution in cur_dir"

let test_reuse_with_overlap () =
  (* Examined set {astring, ocaml} overlaps changed {ocaml} → no reuse *)
  with_tmp_dir @@ fun dir ->
  let prev_dir = Fpath.(dir / "prev") in
  let cur_dir = Fpath.(dir / "cur") in
  ignore (Bos.OS.Dir.create prev_dir);
  ignore (Bos.OS.Dir.create cur_dir);
  let solution = OpamPackage.Map.singleton (pkg "astring.0.8.5") OpamPackage.Set.empty in
  let entry = Incremental_solver.Cached_solution {
    package = pkg "astring.0.8.5";
    result = { Day11_solution.Solve_result.
      packages = OpamPackage.Set.singleton (pkg "astring.0.8.5");
      build_deps = solution;
      doc_deps = solution;
      examined =
        OpamPackage.Name.Set.of_list
          (List.map OpamPackage.Name.of_string [ "astring"; "ocaml" ]);
    };
    cache_key = None;
  } in
  (match Incremental_solver.save Fpath.(prev_dir / "astring.0.8.5.json") entry with
   | Ok () -> () | Error (`Msg e) -> Alcotest.fail e);
  let changed = OpamPackage.Name.Set.singleton
    (OpamPackage.Name.of_string "ocaml") in
  let reused = Incremental_solver.reuse_solutions
    ~solutions_cache_dir:cur_dir ~previous_dir:prev_dir
    ~changed_packages:changed ~packages:["astring.0.8.5"] () in
  Alcotest.(check int) "0 reused" 0 reused

let test_reuse_rekey () =
  (* Rekey mode: entries valid for the previous snapshot's key are
     carried over re-stamped with the new key; entries with a
     different key are not trusted; failures are not carried. *)
  with_tmp_dir @@ fun dir ->
  let prev_dir = Fpath.(dir / "prev") in
  let cur_dir = Fpath.(dir / "cur") in
  ignore (Bos.OS.Dir.create prev_dir);
  ignore (Bos.OS.Dir.create cur_dir);
  let solution =
    OpamPackage.Map.singleton (pkg "astring.0.8.5") OpamPackage.Set.empty in
  let mk_solution ?cache_key name =
    Incremental_solver.Cached_solution {
      package = pkg name;
      result = { Day11_solution.Solve_result.
        packages = OpamPackage.Set.singleton (pkg name);
        build_deps = solution;
        doc_deps = solution;
        examined =
          OpamPackage.Name.Set.of_list
            (List.map OpamPackage.Name.of_string [ "astring"; "ocaml" ]);
      };
      cache_key;
    }
  in
  let save name entry =
    match Incremental_solver.save Fpath.(prev_dir / (name ^ ".json")) entry with
    | Ok () -> () | Error (`Msg e) -> Alcotest.fail e
  in
  save "astring.0.8.5" (mk_solution ~cache_key:"prev-key" "astring.0.8.5");
  save "fmt.0.9.0" (mk_solution ~cache_key:"other-key" "fmt.0.9.0");
  save "broken.1.0" (Incremental_solver.Cached_failure {
    package = pkg "broken.1.0";
    error = "no solution";
    examined = OpamPackage.Name.Set.empty;
    cache_key = Some "prev-key";
  });
  let changed = OpamPackage.Name.Set.singleton
    (OpamPackage.Name.of_string "dune") in
  let reused = Incremental_solver.reuse_solutions
    ~expected_cache_key:"prev-key" ~rekey_to:"new-key"
    ~solutions_cache_dir:cur_dir ~previous_dir:prev_dir
    ~changed_packages:changed
    ~packages:["astring.0.8.5"; "fmt.0.9.0"; "broken.1.0"] () in
  (* astring (solution) and broken (failure) carry; fmt has the wrong
     key and is not trusted. *)
  Alcotest.(check int) "2 reused" 2 reused;
  (match Incremental_solver.load Fpath.(cur_dir / "astring.0.8.5.json") with
   | Ok (Cached_solution s) ->
     Alcotest.(check (option string)) "re-stamped key"
       (Some "new-key") s.cache_key
   | _ -> Alcotest.fail "expected rekeyed solution in cur_dir");
  Alcotest.(check bool) "fmt not carried" false
    (Sys.file_exists (Fpath.to_string Fpath.(cur_dir / "fmt.0.9.0.json")));
  (match Incremental_solver.load Fpath.(cur_dir / "broken.1.0.json") with
   | Ok (Cached_failure f) ->
     Alcotest.(check (option string)) "failure re-stamped"
       (Some "new-key") f.cache_key
   | _ -> Alcotest.fail "expected rekeyed failure in cur_dir");
  (* Overwrite semantics: a stale existing file is replaced. *)
  (match Incremental_solver.save Fpath.(cur_dir / "astring.0.8.5.json")
           (mk_solution ~cache_key:"stale-key" "astring.0.8.5") with
   | Ok () -> () | Error (`Msg e) -> Alcotest.fail e);
  let reused2 = Incremental_solver.reuse_solutions
    ~expected_cache_key:"prev-key" ~rekey_to:"new-key"
    ~solutions_cache_dir:cur_dir ~previous_dir:prev_dir
    ~changed_packages:changed ~packages:["astring.0.8.5"] () in
  Alcotest.(check int) "overwrote stale entry" 1 reused2;
  match Incremental_solver.load Fpath.(cur_dir / "astring.0.8.5.json") with
  | Ok (Cached_solution s) ->
    Alcotest.(check (option string)) "stale key replaced"
      (Some "new-key") s.cache_key
  | _ -> Alcotest.fail "expected rekeyed solution after overwrite"

let test_reuse_tool_stems () =
  (* Tool entries use [<pkg>@<compiler>.json] stems: the package must
     come from the JSON body (the stem doesn't parse as a package),
     and rekey-reuse must work across such filenames. *)
  with_tmp_dir @@ fun dir ->
  let prev_dir = Fpath.(dir / "prev") in
  let cur_dir = Fpath.(dir / "cur") in
  ignore (Bos.OS.Dir.create prev_dir);
  ignore (Bos.OS.Dir.create cur_dir);
  let key_prev = Incremental_solver.tool_cache_key
      ~repos:[ ("repo", "aaaa") ] in
  let key_cur = Incremental_solver.tool_cache_key
      ~repos:[ ("repo", "bbbb") ] in
  Alcotest.(check bool) "keys differ per repo state" true
    (key_prev <> key_cur);
  let solution =
    OpamPackage.Map.singleton (pkg "odoc.3.1.0") OpamPackage.Set.empty in
  let entry = Incremental_solver.Cached_solution {
    package = pkg "odoc.3.1.0";
    result = { Day11_solution.Solve_result.
      packages = OpamPackage.Set.singleton (pkg "odoc.3.1.0");
      build_deps = solution;
      doc_deps = solution;
      examined =
        OpamPackage.Name.Set.of_list
          (List.map OpamPackage.Name.of_string [ "odoc"; "ocaml" ]);
    };
    cache_key = Some key_prev;
  } in
  let stem = "odoc.3.1.0@ocaml-base-compiler.5.2.0" in
  (match Incremental_solver.save Fpath.(prev_dir / (stem ^ ".json")) entry with
   | Ok () -> () | Error (`Msg e) -> Alcotest.fail e);
  let changed = OpamPackage.Name.Set.singleton
    (OpamPackage.Name.of_string "dune") in
  let reused = Incremental_solver.reuse_solutions
    ~expected_cache_key:key_prev ~rekey_to:key_cur
    ~solutions_cache_dir:cur_dir ~previous_dir:prev_dir
    ~changed_packages:changed ~packages:[ stem ] () in
  Alcotest.(check int) "1 reused" 1 reused;
  match Incremental_solver.load Fpath.(cur_dir / (stem ^ ".json")) with
  | Ok (Cached_solution s) ->
    Alcotest.(check string) "package from body"
      "odoc.3.1.0" (OpamPackage.to_string s.package);
    Alcotest.(check (option string)) "rekeyed" (Some key_cur) s.cache_key
  | _ -> Alcotest.fail "expected reused tool entry"

let test_find_previous_sha_dir () =
  with_tmp_dir @@ fun dir ->
  let sha1 = Fpath.(dir / "abc123") in
  let sha2 = Fpath.(dir / "def456") in
  ignore (Bos.OS.Dir.create sha1);
  Unix.sleepf 0.05;
  ignore (Bos.OS.Dir.create sha2);
  (* sha2 is newer — should be found when current is abc123 *)
  match Incremental_solver.find_previous_sha_dir dir ~current_sha:"abc123" with
  | Some p ->
    Alcotest.(check string) "found def456"
      "def456" (Fpath.basename p)
  | None -> Alcotest.fail "expected to find previous SHA dir"

let test_find_previous_sha_dir_none () =
  with_tmp_dir @@ fun dir ->
  let sha1 = Fpath.(dir / "abc123") in
  ignore (Bos.OS.Dir.create sha1);
  match Incremental_solver.find_previous_sha_dir dir ~current_sha:"abc123" with
  | Some _ -> Alcotest.fail "expected None"
  | None -> ()

(* [Summary.record_history] was removed when history writes moved
   into [Recorder] (one-per-outcome rather than bulk-at-tick-end);
   the corresponding tests would now exercise [Recorder.record_build]
   / [record_doc] and live in test_recorder.ml if/when that file
   exists. *)

(* ── Test registration ───────────────────────────────────────────── *)

(* ── Targets tests ──────────────────────────────────────────────── *)

let test_small_universe_nonempty () =
  Alcotest.(check bool) "non-empty" true
    (List.length Targets.small_universe > 0)

let test_small_universe_has_odoc () =
  Alcotest.(check bool) "has odoc" true
    (List.mem "odoc" Targets.small_universe)

let test_resolve_single_target () =
  (* resolve with an explicit target returns exactly that *)
  let dummy_packages = Day11_opam.Git_packages.empty in
  let result = Targets.resolve dummy_packages (Some "astring.0.8.5") in
  Alcotest.(check int) "one target" 1 (List.length result);
  Alcotest.(check string) "correct" "astring.0.8.5"
    (OpamPackage.to_string (List.hd result))

(* Summary.record_history goes through History.append, which uses an
   Eio.Mutex — wrap the whole run in Eio_main.run so the mutex can
   block cooperatively. *)
let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "day11_batch"
    [
      ( "Blessing",
        [
          Alcotest.test_case "single universe" `Quick
            test_blessing_single_universe;
          Alcotest.test_case "two universes" `Quick
            test_blessing_two_universes;
          Alcotest.test_case "empty" `Quick test_blessing_empty;
          Alcotest.test_case "compiler tiebreaker" `Quick
            test_blessing_compiler_tiebreaker;
          Alcotest.test_case "blessed universes" `Quick
            test_blessed_universes;
        ] );
      ( "Dag_executor",
        [
          Alcotest.test_case "basic" `Quick test_dag_executor_basic;
          Alcotest.test_case "failure cascade" `Quick
            test_dag_executor_failure_cascade;
        ] );
      ( "Incremental_solver",
        [
          Alcotest.test_case "save/load solution" `Quick
            test_save_load_solution;
          Alcotest.test_case "save/load failure" `Quick
            test_save_load_failure;
          Alcotest.test_case "reuse no overlap" `Quick
            test_reuse_no_overlap;
          Alcotest.test_case "reuse with overlap" `Quick
            test_reuse_with_overlap;
          Alcotest.test_case "reuse rekey" `Quick test_reuse_rekey;
          Alcotest.test_case "reuse tool stems" `Quick
            test_reuse_tool_stems;
          Alcotest.test_case "find previous SHA dir" `Quick
            test_find_previous_sha_dir;
          Alcotest.test_case "find previous SHA dir none" `Quick
            test_find_previous_sha_dir_none;
        ] );
      ( "Targets",
        [
          Alcotest.test_case "small universe nonempty" `Quick
            test_small_universe_nonempty;
          Alcotest.test_case "small universe has odoc" `Quick
            test_small_universe_has_odoc;
          Alcotest.test_case "resolve single target" `Quick
            test_resolve_single_target;
        ] );
    ]
