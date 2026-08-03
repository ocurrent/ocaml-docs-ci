(* Tests for the day11_layer library (generic, no opam). *)

open Day11_layer
open Day11_test_util.Test_util

let is_ok msg r = ok_or_fail msg r |> ignore

(* ── Meta tests ────────────────────────────────────────────── *)

let test_layer_meta_roundtrip () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let path = Fpath.(dir / "layer.json") in
  let meta : Meta.t =
    {
      exit_status = -1;
      parent_hashes = [ "abc123"; "def456" ];
      uid = 1000;
      gid = 1000;
      base_hash = "test";
      disk_usage = 0;
      timing = Meta.empty_timing;
      created_at = "2024-01-01T00:00:00Z";
      failed_dep = None;
    }
  in
  Meta.save env path meta |> is_ok "save";
  let m = Meta.load env path |> ok_or_fail "load" in
  Alcotest.(check int) "exit_status" (-1) m.exit_status;
  Alcotest.(check (list string))
    "parents" [ "abc123"; "def456" ] m.parent_hashes

(* ── Dir tests ─────────────────────────────────────────────── *)

let test_layer_dir_name () =
  Alcotest.(check string)
    "12-char hash" "c9f7404f9f87"
    (Dir.name "c9f7404f9f87a8b3c4d5e6f7");
  Alcotest.(check string) "short hash" "abc" (Dir.name "abc")

(* ── Symlinks tests ────────────────────────────────────────── *)

let symlink_exists path =
  try (Unix.lstat (Fpath.to_string path)).Unix.st_kind = Unix.S_LNK
  with Unix.Unix_error _ -> false

let test_symlink_create () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let packages_dir = Fpath.(dir / "packages") in
  Symlinks.ensure env ~packages_dir ~id:"yojson.2.2.2"
    ~layer_name:"build-abc123"
  |> is_ok "create";
  let link = Fpath.(packages_dir / "yojson.2.2.2" / "build-abc123") in
  Alcotest.(check bool) "symlink exists" true (symlink_exists link)

let test_symlink_idempotent () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let packages_dir = Fpath.(dir / "packages") in
  Symlinks.ensure env ~packages_dir ~id:"yojson.2.2.2"
    ~layer_name:"build-abc123"
  |> is_ok "first";
  Symlinks.ensure env ~packages_dir ~id:"yojson.2.2.2"
    ~layer_name:"build-abc123"
  |> is_ok "second"

(* ── Scan tests ──────────────────────────────────────────────────── *)

let test_list_layers () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  mkdir Fpath.(dir / "build-abc123");
  mkdir Fpath.(dir / "doc-def456");
  mkdir Fpath.(dir / "packages");
  write_file Fpath.(dir / "somefile") "not a dir";
  let layers = Scan.list_layers env dir in
  let names = List.map fst layers in
  Alcotest.(check bool) "has build" true (List.mem "build-abc123" names);
  Alcotest.(check bool) "has doc" true (List.mem "doc-def456" names);
  Alcotest.(check bool) "has packages" true (List.mem "packages" names);
  Alcotest.(check bool) "no file" false (List.mem "somefile" names)

let test_list_layers_empty () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let layers = Scan.list_layers env Fpath.(dir / "nonexistent") in
  Alcotest.(check (list string)) "empty" [] (List.map fst layers)

let test_list_package_symlinks () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let pkg_dir = Fpath.(dir / "yojson.2.2.2") in
  mkdir pkg_dir;
  Unix.symlink "../../build-abc" (Fpath.to_string Fpath.(pkg_dir / "build-abc"));
  Unix.symlink "../../doc-def" (Fpath.to_string Fpath.(pkg_dir / "doc-def"));
  Unix.symlink "../../build-abc"
    (Fpath.to_string Fpath.(pkg_dir / "blessed-build"));
  let all = Scan.list_package_symlinks env dir "yojson.2.2.2" in
  Alcotest.(check int) "3 total" 3 (List.length all);
  let filtered =
    Scan.list_package_symlinks
      ~exclude:[ "blessed-build"; "blessed-docs" ]
      env dir "yojson.2.2.2"
  in
  let names = List.map fst filtered in
  Alcotest.(check bool) "has build" true (List.mem "build-abc" names);
  Alcotest.(check bool) "has doc" true (List.mem "doc-def" names);
  Alcotest.(check bool) "no blessed" false (List.mem "blessed-build" names)

(* ── Stack tests ─────────────────────────────────────────────────── *)

let test_stack_empty () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "lower") in
  mkdir target;
  Stack.merge ~sw env ~layer_dirs:[] ~target |> is_ok "empty merge"

let test_stack_skips_missing_fs () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let layer = Fpath.(dir / "build-abc") in
  mkdir layer;
  let target = Fpath.(dir / "lower") in
  mkdir target;
  Stack.merge ~sw env ~layer_dirs:[ layer ] ~target |> is_ok "skip no fs"

let test_stack_single_layer () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let layer = Fpath.(dir / "build-aaa") in
  mkdir Fpath.(layer / "fs" / "usr" / "lib");
  write_file Fpath.(layer / "fs" / "usr" / "lib" / "libfoo.a") "content";
  write_file Fpath.(layer / "fs" / "hello.txt") "world";
  let target = Fpath.(dir / "lower") in
  mkdir target;
  Stack.merge ~sw env ~layer_dirs:[ layer ] ~target |> is_ok "merge";
  Alcotest.(check bool)
    "hello.txt" true
    (Bos.OS.File.exists Fpath.(target / "hello.txt") |> Result.get_ok);
  Alcotest.(check bool)
    "nested file" true
    (Bos.OS.File.exists Fpath.(target / "usr" / "lib" / "libfoo.a")
    |> Result.get_ok)

let test_stack_multiple_layers () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let layer1 = Fpath.(dir / "build-aaa") in
  mkdir Fpath.(layer1 / "fs");
  write_file Fpath.(layer1 / "fs" / "base.txt") "from-layer1";
  write_file Fpath.(layer1 / "fs" / "shared.txt") "from-layer1";
  let layer2 = Fpath.(dir / "build-bbb") in
  mkdir Fpath.(layer2 / "fs");
  write_file Fpath.(layer2 / "fs" / "extra.txt") "from-layer2";
  write_file Fpath.(layer2 / "fs" / "shared.txt") "from-layer2";
  let target = Fpath.(dir / "lower") in
  mkdir target;
  Stack.merge ~sw env ~layer_dirs:[ layer1; layer2 ] ~target |> is_ok "merge";
  Alcotest.(check bool)
    "base.txt" true
    (Bos.OS.File.exists Fpath.(target / "base.txt") |> Result.get_ok);
  Alcotest.(check bool)
    "extra.txt" true
    (Bos.OS.File.exists Fpath.(target / "extra.txt") |> Result.get_ok);
  let content =
    Bos.OS.File.read Fpath.(target / "shared.txt") |> Result.get_ok
  in
  Alcotest.(check string) "first layer wins" "from-layer1" content

(* ── plan_lowerdir tests ─────────────────────────────────────────── *)

let fake_layers ~prefix n =
  List.init n (fun i -> Fpath.v (Printf.sprintf "%s/build-%012d" prefix i))

let entry_cost d = String.length (Fpath.to_string Fpath.(d / "fs")) + 1

let test_plan_lowerdir_all_separate () =
  let layers = fake_layers ~prefix:"/c" 10 in
  let separate, to_merge =
    Stack.plan_lowerdir ~available:3900 ~merged_overhead:30 ~entry_cost layers
  in
  Alcotest.(check int) "all kept separate" 10 (List.length separate);
  Alcotest.(check int) "nothing to merge" 0 (List.length to_merge)

let test_plan_lowerdir_split () =
  let prefix = "/home/jjl25/cache/debian-bookworm-x86_64" in
  let layers = fake_layers ~prefix 200 in
  let separate, to_merge =
    Stack.plan_lowerdir ~available:3900 ~merged_overhead:35 ~entry_cost layers
  in
  Alcotest.(check int)
    "total preserved" 200
    (List.length separate + List.length to_merge);
  Alcotest.(check bool) "did split" true (to_merge <> []);
  Alcotest.(check bool) "kept some separate" true (separate <> []);
  let sep_cost = List.fold_left (fun a d -> a + entry_cost d) 0 separate in
  Alcotest.(check bool)
    (Printf.sprintf "separate cost %d + merged %d ≤ available 3900" sep_cost 35)
    true
    (sep_cost + 35 <= 3900)

let test_plan_lowerdir_empty () =
  let separate, to_merge =
    Stack.plan_lowerdir ~available:3900 ~merged_overhead:30 ~entry_cost []
  in
  Alcotest.(check int) "separate" 0 (List.length separate);
  Alcotest.(check int) "to_merge" 0 (List.length to_merge)

let test_plan_lowerdir_short_paths_no_split () =
  let layers = fake_layers ~prefix:"/c" 150 in
  let separate, to_merge =
    Stack.plan_lowerdir ~available:3900 ~merged_overhead:30 ~entry_cost layers
  in
  Alcotest.(check int) "all separate" 150 (List.length separate);
  Alcotest.(check int) "no merge" 0 (List.length to_merge)

let test_plan_lowerdir_boundary () =
  let layers = fake_layers ~prefix:"/c" 5 in
  let per = entry_cost (List.hd layers) in
  let separate, to_merge =
    Stack.plan_lowerdir ~available:(5 * per) ~merged_overhead:1 ~entry_cost
      layers
  in
  Alcotest.(check int) "all fit at boundary" 5 (List.length separate);
  Alcotest.(check int) "no merge" 0 (List.length to_merge);
  let separate, to_merge =
    Stack.plan_lowerdir
      ~available:((5 * per) - 1)
      ~merged_overhead:0 ~entry_cost layers
  in
  Alcotest.(check int) "one short" 4 (List.length separate);
  Alcotest.(check int) "one merged" 1 (List.length to_merge)

(* ── Snapshot tests ──────────────────────────────────────────────── *)

let test_snapshot_empty () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let snap = Snapshot.take env dir in
  Alcotest.(check int) "size" 0 (Snapshot.size snap);
  Alcotest.(check bool) "is_empty" true (Snapshot.is_empty snap)

let test_snapshot_diff_new_file () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "existing") "before";
  let before = Snapshot.take env dir in
  write_file Fpath.(dir / "new-file") "after";
  let changed = Snapshot.diff env ~before dir in
  Alcotest.(check (list string)) "only new file" [ "new-file" ] changed

let test_snapshot_diff_modified () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "f") "one";
  let before = Snapshot.take env dir in
  (* touch to ensure mtime advances *)
  Unix.sleep 1;
  write_file Fpath.(dir / "f") "two";
  let changed = Snapshot.diff env ~before dir in
  Alcotest.(check (list string)) "modified" [ "f" ] changed

let test_snapshot_diff_unchanged () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "f") "same";
  let before = Snapshot.take env dir in
  let changed = Snapshot.diff env ~before dir in
  Alcotest.(check (list string)) "no change" [] changed

(* ── Relocations tests ───────────────────────────────────────────── *)

let test_relocations_clean () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "file") "hello world";
  let hits = Relocations.scan env ~layer_fs:dir ~forbidden:[ "/tmp/abc" ] in
  Alcotest.(check int) "no hits" 0 (List.length hits)

let test_relocations_hit () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  mkdir Fpath.(dir / "bin");
  write_file
    Fpath.(dir / "bin" / "tool")
    "the path is /tmp/build_xyz/prefix/lib";
  let hits =
    Relocations.scan env ~layer_fs:dir ~forbidden:[ "/tmp/build_xyz" ]
  in
  Alcotest.(check int) "one file with hit" 1 (List.length hits);
  match hits with
  | [ (rel, _) ] -> Alcotest.(check string) "rel path" "bin/tool" rel
  | _ -> Alcotest.fail "expected one hit"

let test_relocations_roundtrip () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let data = [ ("bin/ocaml", [ "/tmp/build_xyz" ]) ] in
  let path = Fpath.(dir / "relocations.json") in
  Relocations.save env path data |> is_ok "save";
  match Relocations.load env path with
  | None -> Alcotest.fail "load failed"
  | Some rs -> (
      Alcotest.(check int) "one entry" 1 (List.length rs);
      match rs with
      | [ (k, v) ] ->
          Alcotest.(check string) "key" "bin/ocaml" k;
          Alcotest.(check (list string)) "hits" [ "/tmp/build_xyz" ] v
      | _ -> Alcotest.fail "mismatch")

(* ── Last_used tests ───────────────────────────────────────── *)

(* A layer that has been touched dates from its sentinel. *)
let test_last_used_sentinel () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  Last_used.touch env dir;
  match Last_used.effective env dir with
  | None -> Alcotest.fail "no timestamp for a touched layer"
  | Some t ->
      Alcotest.(check bool)
        "sentinel mtime is recent" true
        (Unix.gettimeofday () -. t < 60.)

(* The regression that emptied the odoc/odoc-driver tool layers out of a
   live cache: [touch] only fires when a layer is re-used, so a freshly
   built layer has no sentinel. Dating it from epoch 0 made every LRU
   sweep eligible to delete it, however new it was. *)
let test_last_used_falls_back_to_layer_json () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  let meta : Meta.t =
    {
      exit_status = 0;
      parent_hashes = [];
      uid = 0;
      gid = 0;
      base_hash = "test";
      disk_usage = 0;
      timing = Meta.empty_timing;
      created_at = "2024-01-01T00:00:00Z";
      failed_dep = None;
    }
  in
  Meta.save env Fpath.(dir / "layer.json") meta |> is_ok "save";
  Alcotest.(check bool) "no sentinel yet" true (Last_used.get env dir = None);
  match Last_used.effective env dir with
  | None -> Alcotest.fail "untouched layer with layer.json dated as unknown"
  | Some t ->
      Alcotest.(check bool)
        "dated from layer.json, not the epoch" true
        (Unix.gettimeofday () -. t < 60.)

(* Residue with neither sentinel nor metadata stays freely evictable. *)
let test_last_used_none_without_metadata () =
  with_eio @@ fun ~sw:_ env ->
  with_tmp_dir @@ fun dir ->
  Alcotest.(check bool) "no timestamp" true (Last_used.effective env dir = None)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_layer"
    [
      ( "Meta",
        [ Alcotest.test_case "roundtrip" `Quick test_layer_meta_roundtrip ] );
      ("Dir", [ Alcotest.test_case "name" `Quick test_layer_dir_name ]);
      ( "Last_used",
        [
          Alcotest.test_case "sentinel" `Quick test_last_used_sentinel;
          Alcotest.test_case "falls back to layer.json" `Quick
            test_last_used_falls_back_to_layer_json;
          Alcotest.test_case "none without metadata" `Quick
            test_last_used_none_without_metadata;
        ] );
      ( "Symlinks",
        [
          Alcotest.test_case "create" `Quick test_symlink_create;
          Alcotest.test_case "idempotent" `Quick test_symlink_idempotent;
        ] );
      ( "Scan",
        [
          Alcotest.test_case "list_layers" `Quick test_list_layers;
          Alcotest.test_case "list_layers empty" `Quick test_list_layers_empty;
          Alcotest.test_case "list_package_symlinks" `Quick
            test_list_package_symlinks;
        ] );
      ( "Stack",
        [
          Alcotest.test_case "empty" `Quick test_stack_empty;
          Alcotest.test_case "skips missing fs" `Quick
            test_stack_skips_missing_fs;
          Alcotest.test_case "single layer" `Quick test_stack_single_layer;
          Alcotest.test_case "multiple layers" `Quick test_stack_multiple_layers;
          Alcotest.test_case "plan_lowerdir all separate" `Quick
            test_plan_lowerdir_all_separate;
          Alcotest.test_case "plan_lowerdir split" `Quick
            test_plan_lowerdir_split;
          Alcotest.test_case "plan_lowerdir empty" `Quick
            test_plan_lowerdir_empty;
          Alcotest.test_case "plan_lowerdir short paths" `Quick
            test_plan_lowerdir_short_paths_no_split;
          Alcotest.test_case "plan_lowerdir boundary" `Quick
            test_plan_lowerdir_boundary;
        ] );
      ( "Snapshot",
        [
          Alcotest.test_case "empty" `Quick test_snapshot_empty;
          Alcotest.test_case "diff new file" `Quick test_snapshot_diff_new_file;
          Alcotest.test_case "diff modified" `Slow test_snapshot_diff_modified;
          Alcotest.test_case "diff unchanged" `Quick
            test_snapshot_diff_unchanged;
        ] );
      ( "Relocations",
        [
          Alcotest.test_case "clean" `Quick test_relocations_clean;
          Alcotest.test_case "hit" `Quick test_relocations_hit;
          Alcotest.test_case "roundtrip" `Quick test_relocations_roundtrip;
        ] );
    ]
