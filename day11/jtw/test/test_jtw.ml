(* Tests for the day11_jtw library. *)

open Day11_jtw
open Day11_test_util.Test_util

(* ── Gen tests ───────────────────────────────────────────────────── *)

let test_layer_hash_deterministic () =
  let h1 = Gen.compute_layer_hash ~build_hash:"abc" ~tools_hash:"def" in
  let h2 = Gen.compute_layer_hash ~build_hash:"abc" ~tools_hash:"def" in
  Alcotest.(check string) "deterministic" h1 h2

let test_layer_hash_varies () =
  let h1 = Gen.compute_layer_hash ~build_hash:"abc" ~tools_hash:"def" in
  let h2 = Gen.compute_layer_hash ~build_hash:"abc" ~tools_hash:"ghi" in
  Alcotest.(check bool) "different" true (h1 <> h2)

let test_dynamic_cmis_json () =
  let json =
    Gen.generate_dynamic_cmis_json ~dcs_url:"/lib/astring/"
      [ "Astring.cmi"; "Astring_base.cmi"; "Astring__Config.cmi" ]
  in
  Alcotest.(check bool)
    "has dcs_url" true
    (Astring.String.is_infix ~affix:"dcs_url" json);
  Alcotest.(check bool)
    "has toplevel Astring" true
    (Astring.String.is_infix ~affix:"Astring" json);
  Alcotest.(check bool)
    "has Astring_base" true
    (Astring.String.is_infix ~affix:"Astring_base" json)

let test_dynamic_cmis_json_empty () =
  let json = Gen.generate_dynamic_cmis_json ~dcs_url:"/lib/" [] in
  Alcotest.(check bool)
    "valid json" true
    (Astring.String.is_infix ~affix:"dcs_url" json);
  Alcotest.(check bool)
    "empty modules" true
    (Astring.String.is_infix ~affix:"[]" json)

let test_findlib_index () =
  let compiler =
    `Assoc [ ("version", `String "5.2.0"); ("content_hash", `String "abc123") ]
  in
  let json =
    Gen.generate_findlib_index ~compiler
      [ "../../p/astring/0.8.5/hash/lib/astring/META" ]
  in
  Alcotest.(check bool)
    "has compiler" true
    (Astring.String.is_infix ~affix:"5.2.0" json);
  Alcotest.(check bool)
    "has meta path" true
    (Astring.String.is_infix ~affix:"astring/META" json)

let test_findlib_names () =
  let names =
    Gen.findlib_names_of_installed_libs
      [
        "astring/astring.cmi";
        "astring/META";
        "hmap/hmap.cmi";
        "hmap/META";
        "fmt/fmt.cmi" (* no META *);
      ]
  in
  Alcotest.(check (list string)) "findlib names" [ "astring"; "hmap" ] names

let test_findlib_names_empty () =
  let names = Gen.findlib_names_of_installed_libs [] in
  Alcotest.(check (list string)) "empty" [] names

let test_container_script () =
  let script =
    Gen.container_script
      ~pkg:(OpamPackage.of_string "astring.0.8.5")
      ~installed_libs:[ "astring/META"; "astring/astring.cmi" ]
  in
  Alcotest.(check bool)
    "has jtw opam" true
    (Astring.String.is_infix ~affix:"jtw opam" script);
  Alcotest.(check bool)
    "has astring" true
    (Astring.String.is_infix ~affix:"astring" script)

let test_container_script_no_libs () =
  let script =
    Gen.container_script
      ~pkg:(OpamPackage.of_string "binary.1.0")
      ~installed_libs:[ "binary/binary.cmi" ]
  in
  (* No META → no findlib packages → returns "true" *)
  Alcotest.(check string) "no-op" "true" script

let test_content_hash () =
  with_tmp_dir @@ fun dir ->
  let lib = Fpath.to_string dir in
  mkdir Fpath.(dir / "astring");
  write_file Fpath.(dir / "astring" / "astring.cmi") "cmi content";
  write_file Fpath.(dir / "astring" / "META") "meta content";
  let h1 = Gen.compute_content_hash lib in
  let h2 = Gen.compute_content_hash lib in
  Alcotest.(check string) "deterministic" h1 h2;
  Alcotest.(check bool) "16 chars" true (String.length h1 = 16)

let test_jtw_result_to_yojson () =
  let json = Gen.jtw_result_to_yojson Gen.Jtw_success in
  let status = Yojson.Safe.Util.(json |> member "status" |> to_string) in
  Alcotest.(check string) "success" "success" status;
  let json = Gen.jtw_result_to_yojson (Gen.Jtw_failure "oops") in
  let status = Yojson.Safe.Util.(json |> member "status" |> to_string) in
  Alcotest.(check string) "failure" "failure" status

(* ── Tool_layer tests ────────────────────────────────────────────── *)

let test_tool_hash_deterministic () =
  let h1 =
    Tool_layer.layer_hash ~base_hash:"b" ~ocaml_version:"5.2.0"
      ~compiler_hashes:[ "c1" ]
  in
  let h2 =
    Tool_layer.layer_hash ~base_hash:"b" ~ocaml_version:"5.2.0"
      ~compiler_hashes:[ "c1" ]
  in
  Alcotest.(check string) "deterministic" h1 h2

let test_tool_hash_varies () =
  let h1 =
    Tool_layer.layer_hash ~base_hash:"b" ~ocaml_version:"5.1.0"
      ~compiler_hashes:[]
  in
  let h2 =
    Tool_layer.layer_hash ~base_hash:"b" ~ocaml_version:"5.2.0"
      ~compiler_hashes:[]
  in
  Alcotest.(check bool) "different" true (h1 <> h2)

let test_tool_layer_name () =
  let name =
    Tool_layer.layer_name ~base_hash:"b" ~ocaml_version:"5.2.0"
      ~compiler_hashes:[]
  in
  Alcotest.(check bool)
    "starts with jtw-tools-" true
    (Astring.String.is_prefix ~affix:"jtw-tools-" name)

let test_tool_exists_empty () =
  with_tmp_dir @@ fun dir ->
  Alcotest.(check bool) "not exists" false (Tool_layer.exists ~layer_dir:dir)

let test_tool_has_jsoo_empty () =
  with_tmp_dir @@ fun dir ->
  Alcotest.(check bool) "no jsoo" false (Tool_layer.has_jsoo ~layer_dir:dir)

let test_tool_has_worker_empty () =
  with_tmp_dir @@ fun dir ->
  Alcotest.(check bool)
    "no worker" false
    (Tool_layer.has_worker_js ~layer_dir:dir)

let test_tool_packages () =
  Alcotest.(check bool)
    "has js_top_worker" true
    (List.mem "js_top_worker" Tool_layer.jtw_packages)

let test_build_script () =
  let script =
    Tool_layer.build_script
      ~packages:[ "js_of_ocaml"; "js_top_worker-bin" ]
      ~pin_commands:[] ~needs_compiler:false ~compiler_pkg:""
  in
  Alcotest.(check bool)
    "has opam install" true
    (Astring.String.is_infix ~affix:"opam install" script);
  Alcotest.(check bool)
    "has jtw" true
    (Astring.String.is_infix ~affix:"jtw" script)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_jtw"
    [
      ( "Gen",
        [
          Alcotest.test_case "layer hash deterministic" `Quick
            test_layer_hash_deterministic;
          Alcotest.test_case "layer hash varies" `Quick test_layer_hash_varies;
          Alcotest.test_case "dynamic_cmis_json" `Quick test_dynamic_cmis_json;
          Alcotest.test_case "dynamic_cmis_json empty" `Quick
            test_dynamic_cmis_json_empty;
          Alcotest.test_case "findlib_index" `Quick test_findlib_index;
          Alcotest.test_case "findlib_names" `Quick test_findlib_names;
          Alcotest.test_case "findlib_names empty" `Quick
            test_findlib_names_empty;
          Alcotest.test_case "container_script" `Quick test_container_script;
          Alcotest.test_case "container_script no libs" `Quick
            test_container_script_no_libs;
          Alcotest.test_case "content_hash" `Quick test_content_hash;
          Alcotest.test_case "jtw_result_to_yojson" `Quick
            test_jtw_result_to_yojson;
        ] );
      ( "Tool_layer",
        [
          Alcotest.test_case "hash deterministic" `Quick
            test_tool_hash_deterministic;
          Alcotest.test_case "hash varies" `Quick test_tool_hash_varies;
          Alcotest.test_case "layer name" `Quick test_tool_layer_name;
          Alcotest.test_case "exists empty" `Quick test_tool_exists_empty;
          Alcotest.test_case "has_jsoo empty" `Quick test_tool_has_jsoo_empty;
          Alcotest.test_case "has_worker empty" `Quick
            test_tool_has_worker_empty;
          Alcotest.test_case "packages" `Quick test_tool_packages;
          Alcotest.test_case "build_script" `Quick test_build_script;
        ] );
    ]
