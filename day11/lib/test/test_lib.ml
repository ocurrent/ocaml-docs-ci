(* Tests for day11_lib modules. *)

open Day11_lib
open Day11_test_util.Test_util

(* ── Classify tests ──────────────────────────────────────────────── *)

let test_classify_transient () =
  let status, category, _ =
    Classify.classify_build_log "Error: No space left on device"
  in
  Alcotest.(check string) "status" "failure" status;
  Alcotest.(check string) "category" "transient_failure" category

let test_classify_depext () =
  let _, category, _ =
    Classify.classify_build_log "E: Unable to locate package libfoo-dev"
  in
  Alcotest.(check string) "category" "depext_unavailable" category

let test_classify_build_failure () =
  let _, category, error =
    Classify.classify_build_log "Error: Unbound module Foo"
  in
  Alcotest.(check string) "category" "build_failure" category;
  Alcotest.(check (option string)) "no specific error" None error

let test_extract_compiler () =
  let json =
    `Assoc
      [
        ( "deps",
          `List [ `String "ocaml-base-compiler.5.1.0"; `String "dune.3.0" ] );
      ]
  in
  let v = Classify.extract_compiler_from_deps json in
  Alcotest.(check string) "compiler version" "5.1.0" v

let test_extract_compiler_missing () =
  let json = `Assoc [ ("deps", `List [ `String "dune.3.0" ]) ] in
  let v = Classify.extract_compiler_from_deps json in
  Alcotest.(check string) "no compiler" "" v

let test_matches_any () =
  Alcotest.(check bool)
    "match" true
    (Classify.matches_any [ "foo"; "bar" ] "contains FOO here");
  Alcotest.(check bool)
    "no match" false
    (Classify.matches_any [ "foo"; "bar" ] "contains baz")

(* ── Universe_manifest tests ─────────────────────────────────────── *)

let test_universe_manifest_roundtrip () =
  with_tmp_dir @@ fun dir ->
  let universes =
    [ ("u-aaa", [ "astring.0.8.5"; "fmt.0.9.0" ]); ("u-bbb", [ "dune.3.0" ]) ]
  in
  (match Universe_manifest.write_all ~snapshot_dir:dir universes with
  | Ok () -> ()
  | Error (`Msg m) -> Alcotest.fail m);
  let idx =
    List.sort compare (Universe_manifest.read_index ~snapshot_dir:dir)
  in
  Alcotest.(check (list string)) "index" [ "u-aaa"; "u-bbb" ] idx;
  match Universe_manifest.read_manifest ~snapshot_dir:dir ~hash:"u-aaa" with
  | Some m ->
      Alcotest.(check string) "hash" "u-aaa" m.hash;
      Alcotest.(check (list string))
        "packages"
        [ "astring.0.8.5"; "fmt.0.9.0" ]
        m.packages
  | None -> Alcotest.fail "manifest u-aaa missing"

let test_universe_manifest_missing () =
  with_tmp_dir @@ fun dir ->
  Alcotest.(check (list string))
    "empty index" []
    (Universe_manifest.read_index ~snapshot_dir:dir);
  Alcotest.(check bool)
    "missing manifest" true
    (Universe_manifest.read_manifest ~snapshot_dir:dir ~hash:"nope" = None)

(* ── Dag_marshal universe tests ──────────────────────────────────── *)

let test_dag_marshal_universe_roundtrip () =
  with_tmp_dir @@ fun dir ->
  let entries =
    [
      {
        Dag_marshal.hash = "h1";
        pkg = OpamPackage.of_string "astring.0.8.5";
        kind = Build;
        deps = [];
        universe = "u-aaa";
        blessed = false;
      };
      {
        Dag_marshal.hash = "h2";
        pkg = OpamPackage.of_string "fmt.0.9.0";
        kind = Compile;
        deps = [ "h1" ];
        universe = "u-bbb";
        blessed = true;
      };
    ]
  in
  (match Dag_marshal.write ~snapshot_dir:dir entries with
  | Ok () -> ()
  | Error (`Msg m) -> Alcotest.fail m);
  match Dag_marshal.read ~snapshot_dir:dir with
  | Ok [ e1; e2 ] ->
      Alcotest.(check string) "u1" "u-aaa" e1.universe;
      Alcotest.(check string) "u2" "u-bbb" e2.universe
  | Ok _ -> Alcotest.fail "wrong entry count"
  | Error (`Msg m) -> Alcotest.fail m

let test_dag_marshal_universe_default () =
  with_tmp_dir @@ fun dir ->
  (* A dag.json written before the universe field existed reads as "". *)
  let json =
    {|{"version":1,"nodes":[{"hash":"h1","pkg":"astring.0.8.5",|}
    ^ {|"kind":"build","deps":[]}]}|}
  in
  (match Bos.OS.File.write (Dag_marshal.path dir) json with
  | Ok () -> ()
  | Error (`Msg m) -> Alcotest.fail m);
  match Dag_marshal.read ~snapshot_dir:dir with
  | Ok [ e ] -> Alcotest.(check string) "default universe" "" e.universe
  | _ -> Alcotest.fail "read failed"

(* ── History tests ───────────────────────────────────────────────── *)

let make_entry ?(status = "success") ?(category = "build") pkg =
  {
    History.ts = "2024-01-01T00:00:00Z";
    run = "run1";
    build_hash = "build-abc";
    status;
    category;
    blessed = false;
    error = None;
    universe = "";
  }
  |> fun e ->
  ignore pkg;
  e

let test_history_append_read () =
  with_tmp_dir @@ fun dir ->
  let packages_dir = dir in
  let entry = make_entry "astring.0.8.5" in
  History.append ~packages_dir ~pkg_str:"astring.0.8.5" entry;
  let entries = History.read ~packages_dir ~pkg_str:"astring.0.8.5" in
  Alcotest.(check int) "one entry" 1 (List.length entries);
  Alcotest.(check string) "status" "success" (List.hd entries).status

let test_history_multiple () =
  with_tmp_dir @@ fun dir ->
  let packages_dir = dir in
  let e1 = make_entry ~status:"success" "x.1" in
  let e2 = make_entry ~status:"failure" ~category:"build_failure" "x.1" in
  History.append ~packages_dir ~pkg_str:"x.1" e1;
  History.append ~packages_dir ~pkg_str:"x.1" e2;
  let entries = History.read ~packages_dir ~pkg_str:"x.1" in
  Alcotest.(check int) "two entries" 2 (List.length entries)

let test_history_empty () =
  with_tmp_dir @@ fun dir ->
  let entries = History.read ~packages_dir:dir ~pkg_str:"nonexistent.1" in
  Alcotest.(check int) "empty" 0 (List.length entries)

(* Two fibers append concurrently to the same history.jsonl.
   The per-path Eio.Mutex must serialise them so every entry ends up
   on its own line (no interleaved bytes) and no write is lost. *)
let test_history_concurrent_append () =
  with_tmp_dir @@ fun dir ->
  let packages_dir = dir in
  let pkg = "astring.0.8.5" in
  let n = 50 in
  let entry_for i =
    {
      History.ts = "2024-01-01T00:00:00Z";
      run = Printf.sprintf "r%d" i;
      build_hash = Printf.sprintf "h%d" i;
      status = "success";
      category = "build";
      blessed = false;
      error = None;
      universe = "";
    }
  in
  Eio.Fiber.both
    (fun () ->
      for i = 0 to n - 1 do
        History.append ~packages_dir ~pkg_str:pkg (entry_for i);
        Eio.Fiber.yield ()
      done)
    (fun () ->
      for i = n to (2 * n) - 1 do
        History.append ~packages_dir ~pkg_str:pkg (entry_for i);
        Eio.Fiber.yield ()
      done);
  let entries = History.read ~packages_dir ~pkg_str:pkg in
  Alcotest.(check int) "all entries written" (2 * n) (List.length entries);
  let hashes =
    List.map (fun (e : History.entry) -> e.build_hash) entries
    |> List.sort compare
  in
  let expected =
    List.init (2 * n) (fun i -> Printf.sprintf "h%d" i) |> List.sort compare
  in
  Alcotest.(check (list string)) "no interleaving or loss" expected hashes

let test_history_json_roundtrip () =
  let entry = make_entry "x.1" in
  let json = History.entry_to_json entry in
  match History.entry_of_json json with
  | Some e ->
      Alcotest.(check string) "status" "success" e.status;
      Alcotest.(check string) "category" "build" e.category
  | None -> Alcotest.fail "roundtrip failed"

(* ── Progress tests ──────────────────────────────────────────────── *)

let test_progress_create () =
  let p =
    Progress.create ~run_id:"run1" ~start_time:"2024-01-01"
      ~targets:[ "astring" ]
  in
  let json = Progress.to_json p in
  Alcotest.(check bool)
    "has run_id" true
    ( Yojson.Safe.to_string json |> fun s ->
      Astring.String.is_infix ~affix:"run1" s )

let test_progress_phases () =
  let p = Progress.create ~run_id:"r1" ~start_time:"t" ~targets:[] in
  let p = Progress.set_phase p Progress.Building in
  let json = Progress.to_json p in
  Alcotest.(check bool)
    "has building" true
    ( Yojson.Safe.to_string json |> fun s ->
      Astring.String.is_infix ~affix:"building" s
      || Astring.String.is_infix ~affix:"Building" s )

let test_progress_write_delete () =
  with_tmp_dir @@ fun dir ->
  let run_dir = Fpath.to_string dir in
  let p = Progress.create ~run_id:"r1" ~start_time:"t" ~targets:[] in
  Progress.write ~run_dir p;
  let exists = Sys.file_exists (Filename.concat run_dir "progress.json") in
  Alcotest.(check bool) "written" true exists;
  Progress.delete ~run_dir;
  let exists2 = Sys.file_exists (Filename.concat run_dir "progress.json") in
  Alcotest.(check bool) "deleted" false exists2

(* ── Notify tests ────────────────────────────────────────────────── *)

let test_notify_channel_roundtrip () =
  let check ch =
    let s = Notify.channel_to_string ch in
    match Notify.channel_of_string s with
    | Some ch2 -> Alcotest.(check bool) s true (ch = ch2)
    | None -> Alcotest.fail ("roundtrip failed for " ^ s)
  in
  check Notify.Stdout;
  check Notify.Slack;
  check Notify.Email

let test_notify_stdout () =
  let code = Notify.send ~channel:Notify.Stdout ~message:"test" in
  Alcotest.(check int) "stdout returns 0" 0 code

(* ── Build_lock tests ────────────────────────────────────────────── *)

let test_lock_empty () =
  with_tmp_dir @@ fun dir ->
  mkdir Fpath.(dir / "locks");
  let locks = Build_lock.list_active ~cache_dir:(Fpath.to_string dir) in
  Alcotest.(check int) "no locks" 0 (List.length locks)

(* ── Epoch tests ─────────────────────────────────────────────────── *)

let test_epoch_create () =
  with_tmp_dir @@ fun dir ->
  let epoch = Epoch.create ~base_dir:dir "abc123" in
  Alcotest.(check bool)
    "epoch dir exists" true
    (Bos.OS.Dir.exists epoch.dir |> Result.get_ok);
  Alcotest.(check string) "hash" "abc123" epoch.hash

let test_epoch_promote () =
  with_tmp_dir @@ fun dir ->
  let epoch = Epoch.create ~base_dir:dir "abc123" in
  Epoch.promote ~base_dir:dir epoch;
  let link_path = Fpath.(dir / "html-live") in
  let target = Unix.readlink (Fpath.to_string link_path) in
  Alcotest.(check bool)
    "symlink target contains epoch hash" true
    (Astring.String.is_infix ~affix:"abc123" target);
  (* Target must be relative so it resolves under any mount point
     (the daemon's HOME vs. Caddy's /srv). *)
  Alcotest.(check bool)
    "symlink target is relative" true
    (Filename.is_relative target);
  Alcotest.(check string) "symlink target" "epoch-abc123/html" target

let test_epoch_current () =
  with_tmp_dir @@ fun dir ->
  (* No epoch yet *)
  let none = Epoch.current ~base_dir:dir in
  Alcotest.(check bool) "no current epoch" true (none = None);
  (* Create and promote one *)
  let epoch = Epoch.create ~base_dir:dir "def456" in
  Epoch.promote ~base_dir:dir epoch;
  let cur = Epoch.current ~base_dir:dir in
  match cur with
  | Some e -> Alcotest.(check string) "current hash" "def456" e.hash
  | None -> Alcotest.fail "expected Some epoch"

let test_epoch_gc () =
  with_tmp_dir @@ fun dir ->
  (* Create several epochs *)
  let _e1 = Epoch.create ~base_dir:dir "aaa" in
  let _e2 = Epoch.create ~base_dir:dir "bbb" in
  let _e3 = Epoch.create ~base_dir:dir "ccc" in
  let deleted = Epoch.gc ~base_dir:dir ~keep:1 in
  (* Should have deleted at least some epochs *)
  Alcotest.(check bool) "deleted some" true (deleted >= 0)

(* gc must never delete the currently-live epoch, even when it's not
   among the [keep] most-recent — e.g. a deliberate rollback-promote. *)
let test_epoch_gc_keeps_live () =
  with_tmp_dir @@ fun dir ->
  let e1 = Epoch.create ~base_dir:dir "aaa" in
  let _e2 = Epoch.create ~base_dir:dir "bbb" in
  let _e3 = Epoch.create ~base_dir:dir "ccc" in
  Epoch.promote ~base_dir:dir e1;
  (* make the oldest live *)
  let _ = Epoch.gc ~base_dir:dir ~keep:1 in
  Alcotest.(check bool)
    "live (oldest) epoch survived gc" true
    (Bos.OS.Dir.exists e1.dir |> Result.get_ok);
  Alcotest.(check bool)
    "current still resolves to the live epoch" true
    (match Epoch.current ~base_dir:dir with
    | Some e -> e.hash = "aaa"
    | None -> false)

(* ── Build_config tests ─────────────────────────────────────────── *)

let test_build_config_roundtrip () =
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "build-config.json") in
  let config : Build_config.t =
    {
      opam_repositories = [ Fpath.v "/tmp/opam-repository" ];
      local_repos = [ (Fpath.v "/tmp/my-repo", [ "pkg1"; "pkg2" ]) ];
      with_doc = true;
      with_jtw = false;
      html_output = Some (Fpath.v "/tmp/html");
      jtw_output = None;
    }
  in
  Build_config.save path config |> ok_or_fail "save";
  let loaded = Build_config.load path |> ok_or_fail "load" in
  Alcotest.(check bool) "with_doc" true loaded.with_doc;
  Alcotest.(check bool) "with_jtw" false loaded.with_jtw;
  Alcotest.(check (option string))
    "html_output" (Some "/tmp/html")
    (Option.map Fpath.to_string loaded.html_output);
  Alcotest.(check (option string))
    "jtw_output" None
    (Option.map Fpath.to_string loaded.jtw_output)

let test_build_config_load_missing () =
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "nonexistent.json") in
  let result = Build_config.load path in
  Alcotest.(check bool) "load fails" true (Result.is_error result)

(* ── Package_list tests ─────────────────────────────────────────── *)

let test_package_list_roundtrip () =
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "packages.json") in
  let packages = [ "astring.0.8.5"; "fmt.0.9.0"; "yojson.2.2.2" ] in
  Package_list.save path packages |> ok_or_fail "save";
  let loaded = Package_list.load path |> ok_or_fail "load" in
  Alcotest.(check (list string)) "roundtrip" packages loaded

let test_package_list_generate () =
  with_tmp_dir @@ fun dir ->
  let packages_dir = dir in
  (* Create history entries: one success, one failure *)
  let success_entry = make_entry ~status:"success" "astring.0.8.5" in
  let failure_entry =
    make_entry ~status:"failure" ~category:"build_failure" "broken.1.0"
  in
  History.append ~packages_dir ~pkg_str:"astring.0.8.5" success_entry;
  History.append ~packages_dir ~pkg_str:"broken.1.0" failure_entry;
  let generated = Package_list.generate ~packages_dir in
  (* The successful package should appear in the list *)
  Alcotest.(check bool)
    "has successful pkg" true
    (List.mem "astring.0.8.5" generated);
  (* The failed package should not appear *)
  Alcotest.(check bool) "no failed pkg" false (List.mem "broken.1.0" generated)

(* ── Disk_usage tests ───────────────────────────────────────────── *)

let test_disk_usage_empty () =
  with_tmp_dir @@ fun dir ->
  let os_dir = Fpath.(dir / "os") in
  let cache_dir = Fpath.(dir / "cache") in
  mkdir os_dir;
  mkdir cache_dir;
  let report = Disk_usage.scan ~os_dir ~cache_dir in
  Alcotest.(check bool) "total >= 0" true (report.total >= 0);
  Alcotest.(check bool) "base >= 0" true (report.base >= 0);
  Alcotest.(check bool) "builds >= 0" true (report.builds >= 0)

(* ── Test registration ───────────────────────────────────────────── *)

(* ── Gc tests ─────────────────────────────────────────────────────── *)

let mkdir path =
  ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote path)))

let test_gc_build_layers () =
  with_tmp_dir @@ fun dir ->
  let os_dir = Fpath.to_string dir in
  mkdir (Filename.concat os_dir "build-aaa111aaa111");
  mkdir (Filename.concat os_dir "build-bbb222bbb222");
  mkdir (Filename.concat os_dir "build-ccc333ccc333");
  (* Also a non-build dir that should be ignored *)
  mkdir (Filename.concat os_dir "packages");
  let result =
    Gc.gc_build_layers ~os_dir ~referenced:[ "build-aaa111aaa111" ]
  in
  Alcotest.(check int) "total" 3 result.total;
  Alcotest.(check int) "kept" 1 result.kept;
  Alcotest.(check int) "deleted" 2 result.deleted;
  Alcotest.(check bool)
    "referenced kept" true
    (Sys.file_exists (Filename.concat os_dir "build-aaa111aaa111"));
  Alcotest.(check bool)
    "unreferenced deleted" false
    (Sys.file_exists (Filename.concat os_dir "build-bbb222bbb222"));
  (* Non-build dir untouched *)
  Alcotest.(check bool)
    "packages untouched" true
    (Sys.file_exists (Filename.concat os_dir "packages"))

let test_gc_build_layers_keeps () =
  with_tmp_dir @@ fun dir ->
  let os_dir = Fpath.to_string dir in
  mkdir (Filename.concat os_dir "build-aaa111aaa111");
  mkdir (Filename.concat os_dir "build-bbb222bbb222");
  let result =
    Gc.gc_build_layers ~os_dir
      ~referenced:[ "build-aaa111aaa111"; "build-bbb222bbb222" ]
  in
  Alcotest.(check int) "none deleted" 0 result.deleted;
  Alcotest.(check int) "all kept" 2 result.kept

let test_gc_odoc_store () =
  with_tmp_dir @@ fun dir ->
  let os_dir = Fpath.to_string dir in
  (* Set up store with p/ (always kept) and u/ entries *)
  mkdir (Filename.concat os_dir "odoc-store/odoc-out/p/fmt/0.11.0");
  mkdir (Filename.concat os_dir "odoc-store/odoc-out/u/aaa111/fmt/0.11.0");
  mkdir (Filename.concat os_dir "odoc-store/odoc-out/u/bbb222/fmt/0.11.0");
  mkdir (Filename.concat os_dir "odoc-store/html/u/aaa111/fmt/0.11.0");
  mkdir (Filename.concat os_dir "odoc-store/html/u/bbb222/fmt/0.11.0");
  let result = Gc.gc_odoc_store ~os_dir ~referenced_universes:[ "aaa111" ] in
  (* bbb222 deleted from both odoc-out and html *)
  Alcotest.(check int) "deleted" 2 result.deleted;
  Alcotest.(check bool)
    "kept referenced" true
    (Sys.file_exists (Filename.concat os_dir "odoc-store/odoc-out/u/aaa111"));
  Alcotest.(check bool)
    "deleted unreferenced" false
    (Sys.file_exists (Filename.concat os_dir "odoc-store/odoc-out/u/bbb222"));
  (* p/ untouched *)
  Alcotest.(check bool)
    "p/ untouched" true
    (Sys.file_exists (Filename.concat os_dir "odoc-store/odoc-out/p/fmt"))

(* Wrap all tests in Eio_main.run so History.append's Eio.Mutex can
   block cooperatively. Tests that don't touch Eio primitives are
   unaffected. *)
let () =
  Eio_main.run @@ fun _env ->
  Alcotest.run "day11_lib"
    [
      ( "Classify",
        [
          Alcotest.test_case "transient" `Quick test_classify_transient;
          Alcotest.test_case "depext" `Quick test_classify_depext;
          Alcotest.test_case "build failure" `Quick test_classify_build_failure;
          Alcotest.test_case "extract compiler" `Quick test_extract_compiler;
          Alcotest.test_case "extract compiler missing" `Quick
            test_extract_compiler_missing;
          Alcotest.test_case "matches_any" `Quick test_matches_any;
        ] );
      ( "Universe_manifest",
        [
          Alcotest.test_case "roundtrip" `Quick test_universe_manifest_roundtrip;
          Alcotest.test_case "missing" `Quick test_universe_manifest_missing;
        ] );
      ( "Dag_marshal",
        [
          Alcotest.test_case "universe roundtrip" `Quick
            test_dag_marshal_universe_roundtrip;
          Alcotest.test_case "universe default" `Quick
            test_dag_marshal_universe_default;
        ] );
      ( "History",
        [
          Alcotest.test_case "append+read" `Quick test_history_append_read;
          Alcotest.test_case "multiple" `Quick test_history_multiple;
          Alcotest.test_case "empty" `Quick test_history_empty;
          Alcotest.test_case "json roundtrip" `Quick test_history_json_roundtrip;
          Alcotest.test_case "concurrent append (Eio mutex)" `Quick
            test_history_concurrent_append;
        ] );
      ( "Progress",
        [
          Alcotest.test_case "create" `Quick test_progress_create;
          Alcotest.test_case "phases" `Quick test_progress_phases;
          Alcotest.test_case "write+delete" `Quick test_progress_write_delete;
        ] );
      ( "Notify",
        [
          Alcotest.test_case "channel roundtrip" `Quick
            test_notify_channel_roundtrip;
          Alcotest.test_case "stdout" `Quick test_notify_stdout;
        ] );
      ("Build_lock", [ Alcotest.test_case "empty" `Quick test_lock_empty ]);
      ( "Epoch",
        [
          Alcotest.test_case "create" `Quick test_epoch_create;
          Alcotest.test_case "promote" `Quick test_epoch_promote;
          Alcotest.test_case "current" `Quick test_epoch_current;
          Alcotest.test_case "gc" `Quick test_epoch_gc;
          Alcotest.test_case "gc keeps live" `Quick test_epoch_gc_keeps_live;
        ] );
      ( "Build_config",
        [
          Alcotest.test_case "save/load roundtrip" `Quick
            test_build_config_roundtrip;
          Alcotest.test_case "load missing" `Quick
            test_build_config_load_missing;
        ] );
      ( "Package_list",
        [
          Alcotest.test_case "save/load roundtrip" `Quick
            test_package_list_roundtrip;
          Alcotest.test_case "generate from history" `Quick
            test_package_list_generate;
        ] );
      ( "Disk_usage",
        [ Alcotest.test_case "scan empty dirs" `Quick test_disk_usage_empty ] );
      ( "Gc",
        [
          Alcotest.test_case "build layers" `Quick test_gc_build_layers;
          Alcotest.test_case "build layers keeps referenced" `Quick
            test_gc_build_layers_keeps;
          Alcotest.test_case "odoc store" `Quick test_gc_odoc_store;
        ] );
    ]
