(* Tests for Html_publish: HTML kept in doc layers, linked into epochs. *)

open Day11_doc
module T = Day11_test_util.Test_util

let write p contents =
  T.mkdir (Fpath.parent p);
  T.write_file p contents

let read p = Bos.OS.File.read p |> Result.get_ok
let exists p = Sys.file_exists (Fpath.to_string p)
let inode p = (Unix.stat (Fpath.to_string p)).Unix.st_ino

(* A freshly rendered HTML tree, as odoc-driver leaves it: one unit under
   [p/] or [u/], plus the toolchain's root support files. *)
let render dir ~unit ~page =
  write Fpath.(dir // unit / "doc" / "index.html") page;
  write Fpath.(dir // unit / "status.json") "{}";
  write Fpath.(dir / "odoc.css") "css";
  write Fpath.(dir / "fonts" / "a.woff2") "font"

let fmt_unit = Fpath.v "p/fmt/0.11.0"

(* Render [page] for [unit] and capture it into a new layer dir. *)
let make_layer root ~name ?(unit = fmt_unit) ?(key = "tools1") page =
  let src = Fpath.(root / (name ^ ".html-out")) in
  render src ~unit ~page;
  let layer_dir = Fpath.(root / "layers" / name) in
  T.mkdir layer_dir;
  T.ok_or_fail "capture"
    (Html_publish.capture ~src ~layer_dir
       ~support_root:Fpath.(root / "support")
       ~support_key:key);
  layer_dir

let publish root ~epoch ~hash layer =
  T.ok_or_fail "publish"
    (Html_publish.publish ~epoch_html:epoch
       ~support_root:Fpath.(root / "support")
       ~hash layer)

let check_published msg ~units ~skipped = function
  | Html_publish.Published p ->
      Alcotest.(check (pair int int)) msg (units, skipped) (p.units, p.skipped)
  | No_html -> Alcotest.failf "%s: expected Published, got No_html" msg

let test_capture_splits_units_and_support () =
  T.with_tmp_dir @@ fun root ->
  let layer = make_layer root ~name:"l1" "v1" in
  let html = Html_publish.layer_html layer in
  Alcotest.(check string)
    "unit in layer" "v1"
    (read Fpath.(html // fmt_unit / "doc" / "index.html"));
  Alcotest.(check bool)
    "no support in layer" false
    (exists Fpath.(html / "odoc.css"));
  Alcotest.(check string)
    "support stored" "css"
    (read Fpath.(root / "support" / "tools1" / "odoc.css"));
  Alcotest.(check bool)
    "scratch consumed" false
    (exists Fpath.(root / "l1.html-out"))

let test_capture_keeps_first_support () =
  T.with_tmp_dir @@ fun root ->
  ignore (make_layer root ~name:"l1" "v1");
  (* A second render with the same toolchain doesn't replace the store. *)
  let src = Fpath.(root / "l2.html-out") in
  render src ~unit:fmt_unit ~page:"v2";
  write Fpath.(src / "odoc.css") "other";
  let layer_dir = Fpath.(root / "layers" / "l2") in
  T.mkdir layer_dir;
  T.ok_or_fail "capture"
    (Html_publish.capture ~src ~layer_dir
       ~support_root:Fpath.(root / "support")
       ~support_key:"tools1");
  Alcotest.(check string)
    "store unchanged" "css"
    (read Fpath.(root / "support" / "tools1" / "odoc.css"))

let test_publish_links_and_skips () =
  T.with_tmp_dir @@ fun root ->
  let layer = make_layer root ~name:"l1" "v1" in
  let epoch = Fpath.(root / "epoch" / "html") in
  check_published "first publish" ~units:1 ~skipped:0
    (publish root ~epoch ~hash:"h1" layer);
  let page = Fpath.(epoch // fmt_unit / "doc" / "index.html") in
  Alcotest.(check string) "page" "v1" (read page);
  Alcotest.(check int)
    "hardlinked, not copied"
    (inode
       Fpath.(Html_publish.layer_html layer // fmt_unit / "doc" / "index.html"))
    (inode page);
  Alcotest.(check string)
    "support linked" "css"
    (read Fpath.(epoch / "odoc.css"));
  Alcotest.(check string)
    "marker" "h1"
    (read Fpath.(epoch // fmt_unit / Html_publish.marker_name));
  check_published "republish is a no-op" ~units:0 ~skipped:1
    (publish root ~epoch ~hash:"h1" layer)

let test_publish_replaces_whole_unit () =
  T.with_tmp_dir @@ fun root ->
  let epoch = Fpath.(root / "epoch" / "html") in
  let old_layer = make_layer root ~name:"old" "old" in
  ignore (publish root ~epoch ~hash:"old" old_layer);
  (* A file only the old build had must not survive the replacement. *)
  write Fpath.(epoch // fmt_unit / "doc" / "Removed" / "index.html") "x";
  let new_layer = make_layer root ~name:"new" "new" in
  check_published "replaced" ~units:1 ~skipped:0
    (publish root ~epoch ~hash:"new" new_layer);
  Alcotest.(check string)
    "new page" "new"
    (read Fpath.(epoch // fmt_unit / "doc" / "index.html"));
  Alcotest.(check bool)
    "stale file gone" false
    (exists Fpath.(epoch // fmt_unit / "doc" / "Removed"));
  Alcotest.(check (list string))
    "no staging leftovers" [ "0.11.0" ]
    (Sys.readdir (Fpath.to_string Fpath.(epoch / "p" / "fmt")) |> Array.to_list)

let test_publish_replaces_legacy_unit () =
  T.with_tmp_dir @@ fun root ->
  let epoch = Fpath.(root / "epoch" / "html") in
  (* Written straight into the epoch by the old bind-mount scheme: no
     marker. *)
  write Fpath.(epoch // fmt_unit / "doc" / "index.html") "legacy";
  let layer = make_layer root ~name:"l1" "v1" in
  check_published "legacy replaced" ~units:1 ~skipped:0
    (publish root ~epoch ~hash:"h1" layer);
  Alcotest.(check string)
    "page" "v1"
    (read Fpath.(epoch // fmt_unit / "doc" / "index.html"))

let test_no_html_layer () =
  T.with_tmp_dir @@ fun root ->
  let layer = Fpath.(root / "layers" / "legacy") in
  T.mkdir Fpath.(layer / "fs");
  match publish root ~epoch:Fpath.(root / "epoch") ~hash:"h" layer with
  | No_html -> ()
  | Published _ -> Alcotest.fail "expected No_html"

let test_unblessed_unit () =
  T.with_tmp_dir @@ fun root ->
  let unit = Fpath.v "u/abc123/fmt/0.11.0" in
  let layer = make_layer root ~name:"l1" ~unit "u" in
  let epoch = Fpath.(root / "epoch") in
  check_published "unblessed" ~units:1 ~skipped:0
    (publish root ~epoch ~hash:"h1" layer);
  Alcotest.(check string)
    "page" "u"
    (read Fpath.(epoch // unit / "doc" / "index.html"))

(* The bug this module exists for: a layer built under profile A is a cache
   hit for profile B. Reconciling B's epoch must still give it the pages,
   and they must outlive the layer. *)
let test_two_epochs_share_a_layer () =
  T.with_tmp_dir @@ fun root ->
  let layer = make_layer root ~name:"shared" "shared" in
  let support_root = Fpath.(root / "support") in
  let epoch_a = Fpath.(root / "a" / "html")
  and epoch_b = Fpath.(root / "b" / "html") in
  ignore (publish root ~epoch:epoch_a ~hash:"hs" layer);
  let st =
    Html_publish.reconcile ~epoch_html:epoch_b ~support_root [ ("hs", layer) ]
  in
  Alcotest.(check int) "linked into b" 1 st.units;
  Alcotest.(check (list (pair string string))) "no errors" [] st.errors;
  ignore (Bos.OS.Path.delete ~recurse:true layer);
  List.iter
    (fun epoch ->
      Alcotest.(check string)
        "survives layer gc" "shared"
        (read Fpath.(epoch // fmt_unit / "doc" / "index.html")))
    [ epoch_a; epoch_b ]

let test_reconcile_self_heals () =
  T.with_tmp_dir @@ fun root ->
  let layer = make_layer root ~name:"l1" "v1" in
  let support_root = Fpath.(root / "support") in
  let epoch = Fpath.(root / "epoch") in
  let run () =
    Html_publish.reconcile ~epoch_html:epoch ~support_root [ ("h1", layer) ]
  in
  ignore (run ());
  ignore (Bos.OS.Path.delete ~recurse:true Fpath.(epoch // fmt_unit));
  let st = run () in
  Alcotest.(check int) "re-linked" 1 st.units;
  Alcotest.(check string)
    "page back" "v1"
    (read Fpath.(epoch // fmt_unit / "doc" / "index.html"))

let () =
  Alcotest.run "html_publish"
    [
      ( "capture",
        [
          Alcotest.test_case "splits units and support" `Quick
            test_capture_splits_units_and_support;
          Alcotest.test_case "keeps first support" `Quick
            test_capture_keeps_first_support;
        ] );
      ( "publish",
        [
          Alcotest.test_case "links and skips" `Quick
            test_publish_links_and_skips;
          Alcotest.test_case "replaces whole unit" `Quick
            test_publish_replaces_whole_unit;
          Alcotest.test_case "replaces legacy unit" `Quick
            test_publish_replaces_legacy_unit;
          Alcotest.test_case "no html" `Quick test_no_html_layer;
          Alcotest.test_case "unblessed unit" `Quick test_unblessed_unit;
        ] );
      ( "reconcile",
        [
          Alcotest.test_case "two epochs share a layer" `Quick
            test_two_epochs_share_a_layer;
          Alcotest.test_case "self-heals" `Quick test_reconcile_self_heals;
        ] );
    ]
