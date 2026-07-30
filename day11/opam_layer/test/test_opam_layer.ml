(* Tests for the day11_opam_layer library. *)

open Day11_opam_layer
open Day11_test_util.Test_util

let is_ok msg r = ok_or_fail msg r |> ignore

let pkg_list ss = List.map OpamPackage.of_string ss

(* ── Build_meta tests ────────────────────────────────────────────── *)

let test_build_meta_roundtrip () = with_tmp_dir @@ fun layer_dir ->
  let m : Build_meta.t = {
    package = "yojson.2.2.2";
    deps = [
      { Build_meta.pkg = "dune.3.0"; hash = "abc123" };
      { Build_meta.pkg = "cppo.1.6"; hash = "def456" };
    ];
    stack = [ "abc123"; "def456"; "ocaml111" ];
    build_deps = [ "cppo.1.6"; "dune.3.0"; "ocaml.5.2.0" ];
    installed_libs = [ "yojson/yojson.cmi"; "yojson/META" ];
    installed_docs = [];
    patches = [];
    base_image = "debian-12-ocaml-5.2";
    cmd = "opam-build -v yojson.2.2.2";
    universe = "u-deadbeef";
  } in
  Build_meta.save layer_dir m |> is_ok "save";
  Alcotest.(check bool) "exists" true (Build_meta.exists layer_dir);
  let loaded = Build_meta.load layer_dir |> ok_or_fail "load" in
  Alcotest.(check string) "package" "yojson.2.2.2" loaded.package;
  Alcotest.(check (list string)) "dep pkgs"
    [ "dune.3.0"; "cppo.1.6" ]
    (List.map (fun (d : Build_meta.dep) -> d.pkg) loaded.deps);
  Alcotest.(check (list string)) "dep hashes"
    [ "abc123"; "def456" ]
    (List.map (fun (d : Build_meta.dep) -> d.hash) loaded.deps);
  Alcotest.(check (list string)) "stack"
    [ "abc123"; "def456"; "ocaml111" ] loaded.stack;
  Alcotest.(check (list string)) "build_deps"
    [ "cppo.1.6"; "dune.3.0"; "ocaml.5.2.0" ] loaded.build_deps;
  Alcotest.(check string) "base_image" "debian-12-ocaml-5.2" loaded.base_image;
  Alcotest.(check string) "cmd" "opam-build -v yojson.2.2.2" loaded.cmd;
  Alcotest.(check string) "universe" "u-deadbeef" loaded.universe;
  Alcotest.(check int) "libs count" 2 (List.length loaded.installed_libs)

(* Backward-compat: an old build.json with deps as a plain string
   list must still load. *)
let test_build_meta_legacy_deps () = with_tmp_dir @@ fun layer_dir ->
  let legacy_json = {|{
    "package": "yojson.2.2.2",
    "deps": ["dune.3.0", "cppo.1.6"],
    "installed_libs": [],
    "installed_docs": [],
    "patches": []
  }|} in
  let path = Fpath.(layer_dir / "build.json") in
  write_file path legacy_json;
  let loaded = Build_meta.load layer_dir |> ok_or_fail "load legacy" in
  Alcotest.(check (list string)) "dep pkgs from legacy"
    [ "dune.3.0"; "cppo.1.6" ]
    (List.map (fun (d : Build_meta.dep) -> d.pkg) loaded.deps);
  Alcotest.(check (list string)) "dep hashes empty for legacy"
    [ ""; "" ]
    (List.map (fun (d : Build_meta.dep) -> d.hash) loaded.deps)

let test_build_meta_missing () = with_tmp_dir @@ fun layer_dir ->
  Alcotest.(check bool) "exists false" false (Build_meta.exists layer_dir);
  match Build_meta.load layer_dir with
  | Ok _ -> Alcotest.fail "should not load missing"
  | Error _ -> ()

(* ── Installed_files tests ───────────────────────────────────────── *)

let test_scan_libs () = with_tmp_dir @@ fun dir ->
  let lib_dir = Fpath.(dir / "fs" / "home" / "opam" / ".opam" / "default" / "lib" / "yojson") in
  mkdir lib_dir;
  write_file Fpath.(lib_dir / "yojson.cmi") "";
  write_file Fpath.(lib_dir / "yojson.cmxa") "";
  write_file Fpath.(lib_dir / "META") "";
  write_file Fpath.(lib_dir / "README.md") "";
  let files = Installed_files.scan_libs ~layer_dir:dir in
  Alcotest.(check bool) "has cmi"
    true (List.mem "yojson/yojson.cmi" files);
  Alcotest.(check bool) "has META"
    true (List.mem "yojson/META" files);
  Alcotest.(check bool) "no README"
    false (List.mem "yojson/README.md" files)

let test_scan_docs () = with_tmp_dir @@ fun dir ->
  let doc_dir = Fpath.(dir / "fs" / "home" / "opam" / ".opam" / "default" / "doc" / "yojson") in
  mkdir doc_dir;
  write_file Fpath.(doc_dir / "index.mld") "";
  write_file Fpath.(doc_dir / "odoc-config.sexp") "";
  write_file Fpath.(doc_dir / "README") "";
  (* dune installs these as [doc:] files; [odoc-md] renders them as pages *)
  write_file Fpath.(doc_dir / "README.md") "";
  write_file Fpath.(doc_dir / "CHANGES.md") "";
  write_file Fpath.(doc_dir / "LICENSE.md") "";
  mkdir Fpath.(doc_dir / "odoc-pages");
  write_file Fpath.(doc_dir / "odoc-pages" / "tutorial.mld") "";
  write_file Fpath.(doc_dir / "odoc-pages" / "diagram.png") "";
  mkdir Fpath.(doc_dir / "odoc-assets");
  write_file Fpath.(doc_dir / "odoc-assets" / "style.css") "";
  let files = Installed_files.scan_docs ~layer_dir:dir in
  let has what = Alcotest.(check bool) ("has " ^ what) true (List.mem what files) in
  let lacks what =
    Alcotest.(check bool) ("no " ^ what) false (List.mem what files)
  in
  has "yojson/index.mld";
  has "yojson/odoc-config.sexp";
  has "yojson/README.md";
  has "yojson/CHANGES.md";
  has "yojson/LICENSE.md";
  has "yojson/odoc-pages/tutorial.mld";
  (* assets alongside [.mld] pages, and in [odoc-assets/], are used too *)
  has "yojson/odoc-pages/diagram.png";
  has "yojson/odoc-assets/style.css";
  (* extension-less top-level files are dropped by the driver *)
  lacks "yojson/README"

let test_scan_empty_layer () = with_tmp_dir @@ fun dir ->
  let files = Installed_files.scan_libs ~layer_dir:dir in
  Alcotest.(check (list string)) "empty" [] files

(* ── Opam_repo tests ─────────────────────────────────────────────── *)

let test_opam_repo_create () = with_tmp_dir @@ fun dir ->
  let repo_dir = Opam_repo.create dir |> ok_or_fail "create" in
  Alcotest.(check bool) "repo file exists"
    true (Bos.OS.File.exists Fpath.(repo_dir / "repo") |> Result.get_ok)

let test_opam_repo_populate () = with_tmp_dir @@ fun dir ->
  let src_repo = Fpath.(dir / "opam-repo") in
  let pkg_dir = Fpath.(src_repo / "packages" / "yojson" / "yojson.2.2.2") in
  mkdir pkg_dir;
  write_file Fpath.(pkg_dir / "opam") {|opam-version: "2.0"|};
  let tgt_repo = Opam_repo.create dir |> ok_or_fail "create" in
  Opam_repo.populate ~opam_repo:tgt_repo
    ~opam_repositories:[ src_repo ]
    (pkg_list [ "yojson.2.2.2" ])
  |> is_ok "populate";
  let copied = Fpath.(tgt_repo / "packages" / "yojson" / "yojson.2.2.2" / "opam") in
  Alcotest.(check bool) "opam file copied"
    true (Bos.OS.File.exists copied |> Result.get_ok)

let test_snapshot_to_layer_no_patches () = with_tmp_dir @@ fun dir ->
  let src_repo = Fpath.(dir / "src-repo") in
  let pkg_dir = Fpath.(src_repo / "packages" / "fmt" / "fmt.0.9.0") in
  mkdir pkg_dir;
  write_file Fpath.(pkg_dir / "opam")
    {|opam-version: "2.0"
synopsis: "test"
build: ["dune" "build"]
|};
  mkdir Fpath.(pkg_dir / "files");
  write_file Fpath.(pkg_dir / "files" / "extra.txt") "hello";
  let layer_dir = Fpath.(dir / "layer") in
  mkdir layer_dir;
  Opam_repo.snapshot_to_layer ~layer_dir
    ~opam_repositories:[ src_repo ]
    (OpamPackage.of_string "fmt.0.9.0")
  |> is_ok "snapshot";
  let slice_pkg =
    Fpath.(layer_dir / "opam-repository" / "packages" / "fmt" / "fmt.0.9.0")
  in
  Alcotest.(check bool) "opam in slice" true
    (Bos.OS.File.exists Fpath.(slice_pkg / "opam") |> Result.get_ok);
  Alcotest.(check bool) "files copied" true
    (Bos.OS.File.exists Fpath.(slice_pkg / "files" / "extra.txt")
     |> Result.get_ok)

let test_snapshot_to_layer_with_patches () = with_tmp_dir @@ fun dir ->
  let src_repo = Fpath.(dir / "src-repo") in
  let pkg_dir = Fpath.(src_repo / "packages" / "fmt" / "fmt.0.9.0") in
  mkdir pkg_dir;
  write_file Fpath.(pkg_dir / "opam")
    {|opam-version: "2.0"
synopsis: "test"
|};
  let patches_dir = Fpath.(dir / "patches") in
  mkdir patches_dir;
  let patch_path = Fpath.(patches_dir / "fix.patch") in
  write_file patch_path "--- a\n+++ b\n";
  let layer_dir = Fpath.(dir / "layer") in
  mkdir layer_dir;
  Opam_repo.snapshot_to_layer ~layer_dir
    ~opam_repositories:[ src_repo ]
    ~patches:[ patch_path ]
    (OpamPackage.of_string "fmt.0.9.0")
  |> is_ok "snapshot";
  let slice_pkg =
    Fpath.(layer_dir / "opam-repository" / "packages" / "fmt" / "fmt.0.9.0")
  in
  let opam_path = Fpath.(slice_pkg / "opam") in
  let opam_text = Bos.OS.File.read opam_path |> ok_or_fail "read opam" in
  Alcotest.(check bool) "patches: field present"
    true
    (let needle = "patches:" in
     try
       let _ = Str.search_forward (Str.regexp_string needle) opam_text 0 in
       true
     with Not_found -> false);
  Alcotest.(check bool) "patch file copied" true
    (Bos.OS.File.exists Fpath.(slice_pkg / "files" / "000-fix.patch")
     |> Result.get_ok)

(* ── Opamh tests ─────────────────────────────────────────────────── *)

let test_compiler_packages () =
  let names = List.map OpamPackage.Name.to_string Opamh.compiler_packages in
  Alcotest.(check bool) "has ocaml"
    true (List.mem "ocaml" names);
  Alcotest.(check bool) "has ocaml-base-compiler"
    true (List.mem "ocaml-base-compiler" names)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_opam_layer"
    [
      ( "Build_meta",
        [
          Alcotest.test_case "roundtrip" `Quick test_build_meta_roundtrip;
          Alcotest.test_case "missing" `Quick test_build_meta_missing;
          Alcotest.test_case "legacy deps" `Quick test_build_meta_legacy_deps;
        ] );
      ( "Installed_files",
        [
          Alcotest.test_case "scan_libs" `Quick test_scan_libs;
          Alcotest.test_case "scan_docs" `Quick test_scan_docs;
          Alcotest.test_case "empty layer" `Quick test_scan_empty_layer;
        ] );
      ( "Opam_repo",
        [
          Alcotest.test_case "create" `Quick test_opam_repo_create;
          Alcotest.test_case "populate" `Quick test_opam_repo_populate;
          Alcotest.test_case "snapshot_to_layer no patches" `Quick
            test_snapshot_to_layer_no_patches;
          Alcotest.test_case "snapshot_to_layer with patches" `Quick
            test_snapshot_to_layer_with_patches;
        ] );
      ( "Opamh",
        [
          Alcotest.test_case "compiler_packages" `Quick test_compiler_packages;
        ] );
    ]
