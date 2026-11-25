(* Tests for Day11_doc.Doc_modules, the export of every package's module paths. *)

open Day11_doc
open Day11_test_util.Test_util

(* ── Sidebar builders ────────────────────────────────────────────── *)

let node ?(children = []) kind content =
  `Assoc
    [
      ( "node",
        `Assoc
          [
            ("url", `Null);
            ("kind", match kind with Some k -> `String k | None -> `Null);
            ("content", `String content);
          ] );
      ("children", `List children);
    ]

let modl ?children name = node ?children (Some "module") name
let page ?children name = node ?children (Some "leaf-page") name

let library ?children name =
  node ?children None ("Library <code>" ^ name ^ "</code>")

let modules json =
  match Doc_modules.modules_of_sidebar json with
  | Some l -> l
  | None -> Alcotest.fail "not a sidebar"

let strings = Alcotest.(list string)

(* ── modules_of_sidebar ──────────────────────────────────────────── *)

let test_paths () =
  let json =
    `List
      [
        page "astring"
          ~children:
            [
              page "README";
              library "astring"
                ~children:
                  [
                    modl "Astring"
                      ~children:
                        [
                          modl "Char" ~children:[ modl "Ascii" ];
                          node (Some "module-type") "S";
                        ];
                  ];
            ];
      ]
  in
  Alcotest.check strings "shallowest first, then byte order"
    [ "Astring"; "Astring.Char"; "Astring.S"; "Astring.Char.Ascii" ]
    (modules json)

let test_depth_cap () =
  let json =
    `List
      [
        modl "A"
          ~children:[ modl "B" ~children:[ modl "C" ~children:[ modl "D" ] ] ];
      ]
  in
  Alcotest.check strings "depth 3" [ "A"; "A.B"; "A.B.C" ] (modules json)

let test_count_cap () =
  let json = `List (List.init 600 (fun i -> modl (Printf.sprintf "M%03d" i))) in
  let l = modules json in
  Alcotest.(check int) "500 paths" Doc_modules.default_max_paths (List.length l);
  Alcotest.(check string) "first" "M000" (List.hd l)

let test_custom_limits () =
  let json =
    `List
      [
        modl "B"
          ~children:[ modl "C" ~children:[ modl "D" ~children:[ modl "E" ] ] ];
        modl "A" ~children:[ modl "Z" ];
      ]
  in
  let with_limits ?max_depth ?max_paths () =
    match Doc_modules.modules_of_sidebar ?max_depth ?max_paths json with
    | Some l -> l
    | None -> Alcotest.fail "not a sidebar"
  in
  Alcotest.check strings "depth 1" [ "A"; "B" ] (with_limits ~max_depth:1 ());
  Alcotest.check strings "3 paths" [ "A"; "B"; "A.Z" ]
    (with_limits ~max_paths:3 ());
  Alcotest.check strings "default depth"
    [ "A"; "B"; "A.Z"; "B.C"; "B.C.D" ]
    (with_limits ());
  Alcotest.check strings "depth 4"
    [ "A"; "B"; "A.Z"; "B.C"; "B.C.D"; "B.C.D.E" ]
    (with_limits ~max_depth:4 ());
  Alcotest.check_raises "depth 0"
    (Invalid_argument "Doc_modules: max_depth < 1") (fun () ->
      ignore (with_limits ~max_depth:0 ()));
  Alcotest.check_raises "0 paths"
    (Invalid_argument "Doc_modules: max_paths < 1") (fun () ->
      ignore (with_limits ~max_paths:0 ()))

let test_invalid_names () =
  let json =
    `List
      [
        modl "Good";
        modl "A\n";
        modl "lower";
        modl "Bad-name" ~children:[ modl "Child" ];
        modl "Ok'";
      ]
  in
  Alcotest.check strings "only valid names, and nothing under an invalid one"
    [ "Good"; "Ok'" ] (modules json)

let test_source_skipped () =
  let json =
    `List [ modl "A"; node (Some "source") "src" ~children:[ modl "Hidden" ] ]
  in
  Alcotest.check strings "source trees" [ "A" ] (modules json)

let test_no_children () =
  let json =
    `List
      [
        `Assoc
          [
            ( "node",
              `Assoc [ ("kind", `String "module"); ("content", `String "A") ] );
          ];
      ]
  in
  Alcotest.check strings "node without children" [ "A" ] (modules json)

let test_not_a_sidebar () =
  let none name json =
    Alcotest.(check bool) name true (Doc_modules.modules_of_sidebar json = None)
  in
  none "empty list" (`List []);
  none "object" (`Assoc []);
  none "module without a name"
    (`List [ `Assoc [ ("node", `Assoc [ ("kind", `String "module") ]) ] ]);
  none "children not a list"
    (`List [ `Assoc [ ("node", `Assoc []); ("children", `Int 1) ] ])

(* ── collect / write ─────────────────────────────────────────────── *)

let sidebar html_dir name version json =
  let dir = Fpath.(html_dir / "p" / name / version / "doc") in
  mkdir dir;
  write_file Fpath.(dir / "sidebar.json") (Yojson.Safe.to_string json)

let fixture html_dir =
  (* lwt: the highest version, in opam's order (5.10.0 > 5.9.2), wins. *)
  sidebar html_dir "lwt" "5.9.2" (`List [ modl "Old" ]);
  sidebar html_dir "lwt" "5.10.0" (`List [ modl "Lwt"; modl "Lwt_io" ]);
  (* meta: the newest sidebar has no module; fall back to an older one. *)
  sidebar html_dir "meta" "2.0" (`List [ page "meta" ]);
  sidebar html_dir "meta" "1.0" (`List [ modl "Meta" ]);
  (* garbled: an unparsable newest sidebar; fall back as well. *)
  let dir = Fpath.(html_dir / "p" / "garbled" / "2" / "doc") in
  mkdir dir;
  write_file Fpath.(dir / "sidebar.json") "[{\"node\"";
  sidebar html_dir "garbled" "1" (`List [ modl "G" ]);
  (* Left out: no sidebar at all, or none with a module. *)
  mkdir Fpath.(html_dir / "p" / "nodocs" / "1.0");
  sidebar html_dir "pages" "1.0" (`List [ page "pages" ])

let test_collect () =
  with_tmp_dir @@ fun dir ->
  let html_dir = Fpath.(dir / "html") in
  fixture html_dir;
  let got =
    Doc_modules.collect html_dir
    |> List.map (fun (p : Doc_modules.package) ->
        (p.name, p.version, p.modules))
  in
  Alcotest.(check (list (triple string string (list string))))
    "packages"
    [
      ("garbled", "1", [ "G" ]);
      ("lwt", "5.10.0", [ "Lwt"; "Lwt_io" ]);
      ("meta", "1.0", [ "Meta" ]);
    ]
    got

let test_collect_empty () =
  with_tmp_dir @@ fun dir ->
  Alcotest.(check int)
    "no p/ directory" 0
    (List.length (Doc_modules.collect Fpath.(dir / "html")))

let test_to_jsonl () =
  Alcotest.(check string)
    "format"
    "{\"format\":\"ocamlorg-doc-modules\",\"version\":1}\n\
     {\"name\":\"a\",\"version\":\"1\",\"modules\":[\"A\"]}\n\
     {\"name\":\"b\",\"version\":\"2\",\"modules\":[\"B\",\"B.C\"]}\n"
    (Doc_modules.to_jsonl
       [
         { name = "b"; version = "2"; modules = [ "B"; "B.C" ] };
         { name = "a"; version = "1"; modules = [ "A" ] };
       ])

let gunzip path =
  Bos.OS.Cmd.(
    run_out Bos.Cmd.(v "gzip" % "-dc" % p path) |> to_string ~trim:false)
  |> Result.get_ok

let test_write () =
  with_tmp_dir @@ fun dir ->
  let html_dir = Fpath.(dir / "html") in
  fixture html_dir;
  let target = Fpath.(html_dir / Doc_modules.filename) in
  let write () =
    match Doc_modules.write html_dir with
    | Ok r -> r
    | Error e -> Alcotest.fail e
  in
  (match write () with
  | `Written, 3 -> ()
  | _ -> Alcotest.fail "first write: expected `Written, 3 packages");
  Alcotest.(check string)
    "gzipped JSONL"
    (Doc_modules.to_jsonl (Doc_modules.collect html_dir))
    (gunzip target);
  let mtime () = (Unix.stat (Fpath.to_string target)).st_mtime in
  let before = mtime () in
  (match write () with
  | `Unchanged, 3 -> ()
  | _ -> Alcotest.fail "second write: expected `Unchanged");
  Alcotest.(check (float 0.)) "file untouched" before (mtime ());
  sidebar html_dir "new" "1" (`List [ modl "New" ]);
  (match write () with
  | `Written, 4 -> ()
  | _ -> Alcotest.fail "third write: expected `Written, 4 packages");
  Alcotest.(check bool)
    "no temporary file left" false
    (Sys.file_exists (Fpath.to_string target ^ ".tmp"))

let () =
  Alcotest.run "doc_modules"
    [
      ( "modules_of_sidebar",
        [
          Alcotest.test_case "paths" `Quick test_paths;
          Alcotest.test_case "depth cap" `Quick test_depth_cap;
          Alcotest.test_case "count cap" `Quick test_count_cap;
          Alcotest.test_case "custom limits" `Quick test_custom_limits;
          Alcotest.test_case "invalid names" `Quick test_invalid_names;
          Alcotest.test_case "source trees" `Quick test_source_skipped;
          Alcotest.test_case "no children" `Quick test_no_children;
          Alcotest.test_case "not a sidebar" `Quick test_not_a_sidebar;
        ] );
      ( "export",
        [
          Alcotest.test_case "collect" `Quick test_collect;
          Alcotest.test_case "collect, no docs" `Quick test_collect_empty;
          Alcotest.test_case "to_jsonl" `Quick test_to_jsonl;
          Alcotest.test_case "write" `Quick test_write;
        ] );
    ]
