(* Tests for the day11_doc library. *)

open Day11_doc
open Day11_test_util.Test_util

(* ── Helpers ─────────────────────────────────────────────────────── *)

let _is_ok msg r = ok_or_fail msg r |> ignore

(* ── Phase tests ─────────────────────────────────────────────────── *)

let test_phase_to_string () =
  Alcotest.(check string) "all"
    "all" (Phase.phase_to_string Phase.Doc_all);
  Alcotest.(check string) "compile-only"
    "compile-only" (Phase.phase_to_string Phase.Doc_compile_only);
  Alcotest.(check string) "link-and-gen"
    "link-and-gen" (Phase.phase_to_string Phase.Doc_link_only)

let test_doc_result_roundtrip_success () =
  let r = Phase.Doc_success { html_path = "/p/yojson/2.2.2"; blessed = true } in
  let json = Phase.doc_result_to_yojson r in
  let r2 = Phase.doc_result_of_yojson json |> Result.get_ok in
  match r2 with
  | Phase.Doc_success { html_path; blessed } ->
      Alcotest.(check string) "html_path" "/p/yojson/2.2.2" html_path;
      Alcotest.(check bool) "blessed" true blessed
  | _ -> Alcotest.fail "expected Doc_success"

let test_doc_result_roundtrip_failure () =
  let r = Phase.Doc_failure "odoc crashed" in
  let json = Phase.doc_result_to_yojson r in
  let r2 = Phase.doc_result_of_yojson json |> Result.get_ok in
  match r2 with
  | Phase.Doc_failure msg ->
      Alcotest.(check string) "message" "odoc crashed" msg
  | _ -> Alcotest.fail "expected Doc_failure"

let test_doc_result_roundtrip_skipped () =
  let r = Phase.Doc_skipped in
  let json = Phase.doc_result_to_yojson r in
  let r2 = Phase.doc_result_of_yojson json |> Result.get_ok in
  match r2 with
  | Phase.Doc_skipped -> ()
  | _ -> Alcotest.fail "expected Doc_skipped"

(* ── Command tests ───────────────────────────────────────────────── *)

let test_command_generation () =
  let cmd =
    Command.odoc_driver_voodoo
      ~pkg:(OpamPackage.of_string "yojson.2.2.2")
      ~universe:"abc123"
      ~blessed:true
      ~actions:"all"
      ~odoc_bin:"/home/opam/.opam/5.2/bin/odoc"
      ~odoc_md_bin:"/home/opam/.opam/5.2/bin/odoc-md"
  in
  Alcotest.(check bool) "contains yojson"
    true (Astring.String.is_infix ~affix:"yojson" cmd);
  Alcotest.(check bool) "contains all"
    true (Astring.String.is_infix ~affix:"all" cmd);
  Alcotest.(check bool) "contains odoc path"
    true (Astring.String.is_infix ~affix:"/home/opam/.opam/5.2/bin/odoc" cmd)

let test_universe_hash_deterministic () =
  let h1 = Command.compute_universe_hash [ "aaa"; "bbb"; "ccc" ] in
  let h2 = Command.compute_universe_hash [ "aaa"; "bbb"; "ccc" ] in
  Alcotest.(check string) "deterministic" h1 h2

let test_universe_hash_different () =
  let h1 = Command.compute_universe_hash [ "aaa"; "bbb" ] in
  let h2 = Command.compute_universe_hash [ "aaa"; "ccc" ] in
  Alcotest.(check bool) "different" true (h1 <> h2)

(* ── Tool_layer tests ────────────────────────────────────────────── *)

let test_driver_hash_deterministic () =
  let h1 = Tool_layer.driver_layer_hash
    ~base_hash:"base1" ~compiler_hashes:[ "c1"; "c2" ] in
  let h2 = Tool_layer.driver_layer_hash
    ~base_hash:"base1" ~compiler_hashes:[ "c1"; "c2" ] in
  Alcotest.(check string) "deterministic" h1 h2

let test_driver_hash_varies () =
  let h1 = Tool_layer.driver_layer_hash
    ~base_hash:"base1" ~compiler_hashes:[ "c1" ] in
  let h2 = Tool_layer.driver_layer_hash
    ~base_hash:"base1" ~compiler_hashes:[ "c2" ] in
  Alcotest.(check bool) "different" true (h1 <> h2)

let test_driver_layer_name () =
  let name = Tool_layer.driver_layer_name
    ~base_hash:"base1" ~compiler_hashes:[ "c1" ] in
  Alcotest.(check bool) "starts with doc-driver-"
    true (Astring.String.is_prefix ~affix:"doc-driver-" name)

let test_driver_exists_empty () = with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  Alcotest.(check bool) "not exists"
    false (Tool_layer.driver_exists env ~layer_dir:dir)

let test_driver_exists_with_layer_json () = with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "layer.json") "{}";
  Alcotest.(check bool) "exists"
    true (Tool_layer.driver_exists env ~layer_dir:dir)

let test_driver_build_script () =
  let script = Tool_layer.driver_build_script
    ~packages:[ "odoc"; "odoc-driver" ]
    ~pin_commands:[] in
  Alcotest.(check bool) "contains opam install"
    true (Astring.String.is_infix ~affix:"opam install" script);
  Alcotest.(check bool) "contains odoc"
    true (Astring.String.is_infix ~affix:"odoc" script)

let test_odoc_hash_varies_by_version () =
  let h1 = Tool_layer.odoc_layer_hash
    ~base_hash:"b" ~ocaml_version:"5.1.0" ~compiler_hashes:[] in
  let h2 = Tool_layer.odoc_layer_hash
    ~base_hash:"b" ~ocaml_version:"5.2.0" ~compiler_hashes:[] in
  Alcotest.(check bool) "different versions" true (h1 <> h2)

let test_odoc_layer_name () =
  let name = Tool_layer.odoc_layer_name
    ~base_hash:"b" ~ocaml_version:"5.2.0" ~compiler_hashes:[] in
  Alcotest.(check bool) "starts with doc-odoc-"
    true (Astring.String.is_prefix ~affix:"doc-odoc-" name)

(* ── Prep tests ──────────────────────────────────────────────────── *)

let test_prep_create () = with_tmp_dir @@ fun dir ->
  let source = Fpath.(dir / "build-layer") in
  let dest = Fpath.(dir / "doc-layer") in
  (* Create a mock build layer with some .cmti files *)
  let lib_dir = Fpath.(source / "fs" / "home" / "opam" / ".opam" / "5.2" / "lib" / "astring") in
  mkdir lib_dir;
  write_file Fpath.(lib_dir / "astring.cmti") "cmti content";
  write_file Fpath.(lib_dir / "astring.cmt") "cmt content";
  write_file Fpath.(lib_dir / "META") "meta content";
  let prep_root, _mounts =
    Prep.create_with_mounts
      ~source_layer_dir:source
      ~dest_layer_dir:dest
      ~universe:"abc123"
      ~pkg:(OpamPackage.of_string "astring.0.8.5")
      ~installed_libs:[ "astring/astring.cmti"; "astring/astring.cmt"; "astring/META" ]
      ~installed_docs:[]
    |> ok_or_fail "prep"
  in
  Alcotest.(check bool) "prep dir exists"
    true (Bos.OS.Dir.exists prep_root |> Result.get_ok)

(* [voodoo-prep] used to copy the switch's whole [doc/] tree, which is how
   [README.md]/[CHANGES.md] reached [odoc_driver_voodoo]. Check they make it
   into the prep tree at the path the driver classifies them from,
   [doc/<pkg>/<file>]. *)
let test_prep_copies_doc_files () = with_tmp_dir @@ fun dir ->
  let source = Fpath.(dir / "build-layer") in
  let dest = Fpath.(dir / "doc-layer") in
  let doc_dir = Fpath.(source / "fs" / "home" / "opam" / ".opam" / "default"
                       / "doc" / "astring") in
  mkdir Fpath.(doc_dir / "odoc-pages");
  write_file Fpath.(doc_dir / "README.md") "# astring";
  write_file Fpath.(doc_dir / "CHANGES.md") "## v0.8.5";
  write_file Fpath.(doc_dir / "odoc-config.sexp") "()";
  write_file Fpath.(doc_dir / "odoc-pages" / "tutorial.mld") "{0 Tutorial}";
  let prep_root, _mounts =
    Prep.create_with_mounts
      ~source_layer_dir:source
      ~dest_layer_dir:dest
      ~universe:"abc123"
      ~pkg:(OpamPackage.of_string "astring.0.8.5")
      ~installed_libs:[]
      ~installed_docs:[ "astring/README.md"; "astring/CHANGES.md";
                        "astring/odoc-config.sexp";
                        "astring/odoc-pages/tutorial.mld" ]
    |> ok_or_fail "prep"
  in
  let pkg_doc =
    Fpath.(prep_root / "universes" / "abc123" / "astring" / "0.8.5" / "doc")
  in
  let copied rel =
    Alcotest.(check bool) ("copied " ^ rel)
      true (Bos.OS.File.exists Fpath.(pkg_doc // v rel) |> Result.get_ok)
  in
  copied "astring/README.md";
  copied "astring/CHANGES.md";
  copied "astring/odoc-config.sexp";
  copied "astring/odoc-pages/tutorial.mld";
  Alcotest.(check string) "README contents"
    "# astring"
    (Bos.OS.File.read Fpath.(pkg_doc / "astring" / "README.md")
     |> Result.get_ok);
  (* the package is discoverable via its own doc files, so no stub needed *)
  Alcotest.(check bool) "no stub index.mld"
    false
    (Bos.OS.File.exists Fpath.(pkg_doc / "astring" / "index.mld")
     |> Result.get_ok)

let test_prep_empty_libs () = with_tmp_dir @@ fun dir ->
  let source = Fpath.(dir / "build-layer") in
  let dest = Fpath.(dir / "doc-layer") in
  mkdir Fpath.(source / "fs");
  let prep_root, _mounts =
    Prep.create_with_mounts
      ~source_layer_dir:source
      ~dest_layer_dir:dest
      ~universe:"abc123"
      ~pkg:(OpamPackage.of_string "binary-pkg.1.0")
      ~installed_libs:[]
      ~installed_docs:[]
    |> ok_or_fail "prep"
  in
  Alcotest.(check bool) "prep dir exists (even if empty)"
    true (Bos.OS.Dir.exists prep_root |> Result.get_ok)

(* ── Combine tests ───────────────────────────────────────────────── *)

let test_scan_cache_empty () = with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let layers = Combine.scan_cache env ~os_dir:dir in
  Alcotest.(check int) "empty" 0 (List.length layers)

(* ── Sync tests ──────────────────────────────────────────────────── *)

let test_sync_scan_cache_empty () = with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let entries = Sync.scan_cache env ~os_dir:dir in
  Alcotest.(check int) "empty" 0 (List.length entries)

(* ── Universe tests ──────────────────────────────────────────────── *)

let test_universe_manifest_roundtrip () = with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "universes" / "abc123.json") in
  mkdir Fpath.(dir / "universes");
  Universe.save_manifest path
    ~universe_hash:"abc123"
    ~packages:[ "astring.0.8.5"; "fmt.0.9.0" ]
  |> ok_or_fail "save_manifest";
  let (hash, pkgs) =
    Universe.load_manifest path |> ok_or_fail "load_manifest" in
  Alcotest.(check string) "hash" "abc123" hash;
  Alcotest.(check (list string)) "packages"
    [ "astring.0.8.5"; "fmt.0.9.0" ] pkgs

let test_universe_package_refs_roundtrip () = with_tmp_dir @@ fun dir ->
  (* Set up html/p/astring/0.8.5/ directory *)
  let pkg_html_dir = Fpath.(dir / "html" / "p" / "astring" / "0.8.5") in
  mkdir pkg_html_dir;
  Universe.write_package_refs
    ~pkg_html_dir
    ~universe_hashes:[ "abc123"; "def456" ]
  |> ok_or_fail "write_package_refs";
  (* collect_referenced scans html/p/*/... for universes.json *)
  let html_dir = Fpath.(dir / "html") in
  let referenced = Universe.collect_referenced ~html_dir in
  Alcotest.(check bool) "has abc123"
    true (List.mem "abc123" referenced);
  Alcotest.(check bool) "has def456"
    true (List.mem "def456" referenced)

let test_universe_gc () = with_tmp_dir @@ fun dir ->
  let html_dir = Fpath.(dir / "html") in
  (* Create universe dirs under html/u/ *)
  mkdir Fpath.(html_dir / "u" / "referenced");
  mkdir Fpath.(html_dir / "u" / "orphaned");
  (* Create a package that references "referenced" but not "orphaned" *)
  let pkg_html_dir = Fpath.(html_dir / "p" / "pkg" / "1.0") in
  mkdir pkg_html_dir;
  Universe.write_package_refs
    ~pkg_html_dir
    ~universe_hashes:[ "referenced" ]
  |> ok_or_fail "write_package_refs";
  let deleted = Universe.gc ~html_dir in
  (* "orphaned" should be deleted, "referenced" kept *)
  Alcotest.(check bool) "deleted at least one" true (deleted >= 1);
  Alcotest.(check bool) "referenced still exists"
    true (Bos.OS.Dir.exists Fpath.(html_dir / "u" / "referenced")
          |> Result.get_ok);
  Alcotest.(check bool) "orphaned removed"
    false (Bos.OS.Dir.exists Fpath.(html_dir / "u" / "orphaned")
           |> Result.get_ok)

(* ── Generate tests ──────────────────────────────────────────────── *)

let pkg s = OpamPackage.of_string s

let test_find_compiler_base () =
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.5.4.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "ocaml.5.4.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "dune.3.21.1") OpamPackage.Set.empty
  in
  match Generate.find_compiler solution with
  | Some c -> Alcotest.(check string) "found" "ocaml-base-compiler.5.4.1"
    (OpamPackage.to_string c)
  | None -> Alcotest.fail "expected compiler"

let test_find_compiler_variants () =
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-variants.5.2.0+ox") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "ocaml.5.2.0") OpamPackage.Set.empty
  in
  match Generate.find_compiler solution with
  | Some c -> Alcotest.(check string) "found" "ocaml-variants.5.2.0+ox"
    (OpamPackage.to_string c)
  | None -> Alcotest.fail "expected compiler"

let test_find_compiler_none () =
  let solution =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "dune.3.21.1") OpamPackage.Set.empty
  in
  Alcotest.(check bool) "none" true
    (Generate.find_compiler solution = None)

let make_solve_result solution =
  { Day11_solution.Solve_result.
    packages = OpamPackage.Map.fold (fun p _ acc -> OpamPackage.Set.add p acc)
      solution OpamPackage.Set.empty;
    build_deps = solution;
    doc_deps = solution;
    examined = OpamPackage.Name.Set.empty }

let test_unique_compilers () =
  let sol1 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.5.4.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "astring.0.8.5") OpamPackage.Set.empty
  in
  let sol2 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.4.14.2") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "dune.3.21.1") OpamPackage.Set.empty
  in
  let sol3 =
    OpamPackage.Map.empty
    |> OpamPackage.Map.add (pkg "ocaml-base-compiler.5.4.1") OpamPackage.Set.empty
    |> OpamPackage.Map.add (pkg "fmt.0.11.0") OpamPackage.Set.empty
  in
  let compilers = Generate.unique_compilers
    [ (pkg "astring.0.8.5", make_solve_result sol1);
      (pkg "distributed.0.6.0", make_solve_result sol2);
      (pkg "fmt.0.11.0", make_solve_result sol3) ] in
  Alcotest.(check int) "two unique" 2 (List.length compilers);
  let strs = List.map OpamPackage.to_string compilers
    |> List.sort String.compare in
  Alcotest.(check (list string)) "versions"
    [ "ocaml-base-compiler.4.14.2"; "ocaml-base-compiler.5.4.1" ] strs

let test_unique_compilers_empty () =
  let compilers = Generate.unique_compilers [] in
  Alcotest.(check int) "empty" 0 (List.length compilers)

(* ── Doc_deps tests ──────────────────────────────────────────────── *)

(* Helper: build a solution map from a list of (pkg, [dep; dep; ...]) *)
let make_solution entries =
  List.fold_left (fun acc (p, deps) ->
    OpamPackage.Map.add (pkg p)
      (OpamPackage.Set.of_list (List.map pkg deps)) acc
  ) OpamPackage.Map.empty entries

let make_result ?(doc_deps = OpamPackage.Map.empty) build_deps =
  { Day11_solution.Solve_result.
    packages = OpamPackage.Map.fold (fun p _ acc ->
      OpamPackage.Set.add p acc) build_deps OpamPackage.Set.empty;
    build_deps;
    doc_deps;
    examined = OpamPackage.Name.Set.empty }

(* No {post} deps anywhere — compile and link graphs identical.
   Every package can use --actions all. *)
let test_doc_deps_no_post () =
  let deps = make_solution [
    ("fmt.0.11.0", ["ocaml.5.4.1"; "dune.3.21.1"]);
    ("dune.3.21.1", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let result = make_result ~doc_deps:deps deps in
  Alcotest.(check bool) "fmt: single phase" false
    (Doc_deps.needs_separate_link result (pkg "fmt.0.11.0"));
  Alcotest.(check bool) "dune: single phase" false
    (Doc_deps.needs_separate_link result (pkg "dune.3.21.1"))

(* Package has a {post} dep — link graph has an extra dep that the
   compile graph doesn't. That package needs separate phases. *)
let test_doc_deps_with_post_dep () =
  let compile_deps = make_solution [
    ("odoc.3.1.0", ["ocaml.5.4.1"; "dune.3.21.1"]);
    ("dune.3.21.1", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let doc_deps = make_solution [
    ("odoc.3.1.0", ["ocaml.5.4.1"; "dune.3.21.1"; "odoc-parser.3.0.0"]);
    ("dune.3.21.1", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let result = make_result ~doc_deps compile_deps in
  Alcotest.(check bool) "odoc: needs separate" true
    (Doc_deps.needs_separate_link result (pkg "odoc.3.1.0"));
  Alcotest.(check bool) "dune: single phase" false
    (Doc_deps.needs_separate_link result (pkg "dune.3.21.1"))

(* Package not in either graph — no deps to compare, single phase. *)
let test_doc_deps_absent_pkg () =
  let deps = make_solution [
    ("fmt.0.11.0", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let result = make_result ~doc_deps:deps deps in
  Alcotest.(check bool) "absent: single phase" false
    (Doc_deps.needs_separate_link result (pkg "unknown-pkg.1.0"))

(* Single package, no deps in either graph *)
let test_doc_deps_leaf () =
  let deps = make_solution [
    ("astring.0.8.5", []);
  ] in
  let result = make_result ~doc_deps:deps deps in
  Alcotest.(check bool) "leaf: single phase" false
    (Doc_deps.needs_separate_link result (pkg "astring.0.8.5"))

(* Multiple packages in a solution, only the one with {post} deps
   needs separate phases — the others are unaffected. *)
let test_doc_deps_mixed () =
  let compile_deps = make_solution [
    ("yojson.2.2.2", ["ocaml.5.4.1"; "dune.3.21.1"]);
    ("ppx_yojson.1.3.0", ["ocaml.5.4.1"; "dune.3.21.1"; "yojson.2.2.2"]);
    ("dune.3.21.1", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let doc_deps = make_solution [
    ("yojson.2.2.2", ["ocaml.5.4.1"; "dune.3.21.1"]);
    ("ppx_yojson.1.3.0", ["ocaml.5.4.1"; "dune.3.21.1"; "yojson.2.2.2";
                           "ppxlib.0.33.0"]);
    ("dune.3.21.1", ["ocaml.5.4.1"]);
    ("ocaml.5.4.1", []);
  ] in
  let result = make_result ~doc_deps compile_deps in
  Alcotest.(check bool) "yojson: single phase" false
    (Doc_deps.needs_separate_link result (pkg "yojson.2.2.2"));
  Alcotest.(check bool) "ppx_yojson: needs separate" true
    (Doc_deps.needs_separate_link result (pkg "ppx_yojson.1.3.0"));
  Alcotest.(check bool) "dune: single phase" false
    (Doc_deps.needs_separate_link result (pkg "dune.3.21.1"))

(* ── Odoc_store tests ────────────────────────────────────────────── *)

let loc ?(universe = "u1") ?(blessed = true) s : Odoc_store.pkg_loc =
  { pkg = pkg s; universe; blessed }

let test_rel_path_blessed () =
  let p = Odoc_store.rel_path (loc "astring.0.8.5") in
  Alcotest.(check string) "blessed" "p/astring/0.8.5" (Fpath.to_string p)

let test_rel_path_non_blessed () =
  let p = Odoc_store.rel_path (loc ~blessed:false ~universe:"abc123" "astring.0.8.5") in
  Alcotest.(check string) "non-blessed" "u/abc123/astring/0.8.5" (Fpath.to_string p)

let test_container_html () =
  Alcotest.(check string) "container_html" "/home/opam/html"
    Odoc_store.container_html

(* ── Doc_meta tests ─────────────────────────────────────────────── *)

let test_doc_meta_roundtrip () = with_tmp_dir @@ fun layer_dir ->
  let m : Doc_meta.t = {
    package = "fmt.0.9.0";
    phase = Doc_meta.Doc_all;
    deps = [ "ocaml.5.4.1"; "fmt.0.9.0" ];
  } in
  Doc_meta.save layer_dir m |> _is_ok "save";
  Alcotest.(check bool) "exists" true (Doc_meta.exists layer_dir);
  let loaded = Doc_meta.load layer_dir |> ok_or_fail "load" in
  Alcotest.(check string) "package" "fmt.0.9.0" loaded.package;
  Alcotest.(check bool) "phase doc-all" true (loaded.phase = Doc_meta.Doc_all)

let test_doc_meta_phases () = with_tmp_dir @@ fun layer_dir ->
  List.iter (fun phase ->
    let m : Doc_meta.t = { package = "x.1"; phase; deps = [] } in
    Doc_meta.save layer_dir m |> _is_ok "save";
    let loaded = Doc_meta.load layer_dir |> ok_or_fail "load" in
    Alcotest.(check bool) "phase round-trip" true (loaded.phase = phase)
  ) [ Doc_meta.Compile; Doc_meta.Link; Doc_meta.Doc_all ]

let test_doc_meta_missing () = with_tmp_dir @@ fun layer_dir ->
  Alcotest.(check bool) "exists false" false (Doc_meta.exists layer_dir);
  match Doc_meta.load layer_dir with
  | Ok _ -> Alcotest.fail "should not load missing"
  | Error _ -> ()

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_doc"
    [
      ( "Phase",
        [
          Alcotest.test_case "to_string" `Quick test_phase_to_string;
          Alcotest.test_case "result success roundtrip" `Quick
            test_doc_result_roundtrip_success;
          Alcotest.test_case "result failure roundtrip" `Quick
            test_doc_result_roundtrip_failure;
          Alcotest.test_case "result skipped roundtrip" `Quick
            test_doc_result_roundtrip_skipped;
        ] );
      ( "Command",
        [
          Alcotest.test_case "generation" `Quick test_command_generation;
          Alcotest.test_case "universe hash deterministic" `Quick
            test_universe_hash_deterministic;
          Alcotest.test_case "universe hash different" `Quick
            test_universe_hash_different;
        ] );
      ( "Tool_layer",
        [
          Alcotest.test_case "driver hash deterministic" `Quick
            test_driver_hash_deterministic;
          Alcotest.test_case "driver hash varies" `Quick
            test_driver_hash_varies;
          Alcotest.test_case "driver layer name" `Quick
            test_driver_layer_name;
          Alcotest.test_case "driver exists empty" `Quick
            test_driver_exists_empty;
          Alcotest.test_case "driver exists with json" `Quick
            test_driver_exists_with_layer_json;
          Alcotest.test_case "driver build script" `Quick
            test_driver_build_script;
          Alcotest.test_case "odoc hash varies by version" `Quick
            test_odoc_hash_varies_by_version;
          Alcotest.test_case "odoc layer name" `Quick
            test_odoc_layer_name;
        ] );
      ( "Prep",
        [
          Alcotest.test_case "create" `Quick test_prep_create;
          Alcotest.test_case "copies doc files" `Quick
            test_prep_copies_doc_files;
          Alcotest.test_case "empty libs" `Quick test_prep_empty_libs;
        ] );
      ( "Combine",
        [
          Alcotest.test_case "scan empty cache" `Quick test_scan_cache_empty;
        ] );
      ( "Sync",
        [
          Alcotest.test_case "scan empty cache" `Quick
            test_sync_scan_cache_empty;
        ] );
      ( "Universe",
        [
          Alcotest.test_case "manifest roundtrip" `Quick
            test_universe_manifest_roundtrip;
          Alcotest.test_case "package refs roundtrip" `Quick
            test_universe_package_refs_roundtrip;
          Alcotest.test_case "gc removes unreferenced" `Quick
            test_universe_gc;
        ] );
      ( "Generate",
        [
          Alcotest.test_case "find_compiler base" `Quick
            test_find_compiler_base;
          Alcotest.test_case "find_compiler variants" `Quick
            test_find_compiler_variants;
          Alcotest.test_case "find_compiler none" `Quick
            test_find_compiler_none;
          Alcotest.test_case "unique_compilers" `Quick
            test_unique_compilers;
          Alcotest.test_case "unique_compilers empty" `Quick
            test_unique_compilers_empty;
        ] );
      ( "Doc_deps",
        [
          Alcotest.test_case "no post deps" `Quick test_doc_deps_no_post;
          Alcotest.test_case "with post dep" `Quick test_doc_deps_with_post_dep;
          Alcotest.test_case "absent package" `Quick test_doc_deps_absent_pkg;
          Alcotest.test_case "leaf" `Quick test_doc_deps_leaf;
          Alcotest.test_case "mixed" `Quick test_doc_deps_mixed;
        ] );
      ( "Odoc_store",
        [
          Alcotest.test_case "rel_path blessed" `Quick test_rel_path_blessed;
          Alcotest.test_case "rel_path non-blessed" `Quick test_rel_path_non_blessed;
          Alcotest.test_case "container_html" `Quick test_container_html;
        ] );
      ( "Doc_meta",
        [
          Alcotest.test_case "roundtrip" `Quick test_doc_meta_roundtrip;
          Alcotest.test_case "all phases" `Quick test_doc_meta_phases;
          Alcotest.test_case "missing" `Quick test_doc_meta_missing;
        ] );
    ]
