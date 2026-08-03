let jtw_packages =
  [
    "js_top_worker";
    "js_top_worker-rpc";
    "js_top_worker-bin";
    "js_top_worker-web";
    "js_top_worker-widget";
    "js_top_worker-widget-leaflet";
    "js_top_worker-client";
    "js_top_worker-unix";
  ]

let layer_hash ~base_hash ~ocaml_version ~compiler_hashes =
  Day11_layer.Hash.of_strings
    ([ "jtw-tools"; base_hash; ocaml_version ] @ compiler_hashes)

let layer_name ~base_hash ~ocaml_version ~compiler_hashes =
  let hash = layer_hash ~base_hash ~ocaml_version ~compiler_hashes in
  "jtw-tools-" ^ String.sub hash 0 12

let build_script ~packages ~pin_commands ~needs_compiler ~compiler_pkg =
  let compiler_cmds =
    if needs_compiler then [ Printf.sprintf "opam install -y %s" compiler_pkg ]
    else []
  in
  let install_cmd =
    Printf.sprintf "opam install -y %s" (String.concat " " packages)
  in
  String.concat " && "
    (compiler_cmds
    @ pin_commands
    @ [
        install_cmd;
        "eval $(opam env) && which js_of_ocaml && which jtw";
        "eval $(opam env) && jtw opam --no-worker -o \
         /home/opam/jtw-tools-output stdlib";
      ])

type pin = { package : string; url : string }

let build_cmd ~repo ~branch ~extra_pins =
  let jtw_pin_cmds =
    List.map
      (fun pkg ->
        Printf.sprintf "opam pin add -yn %s git+%s#%s" pkg repo branch)
      jtw_packages
  in
  let extra_pin_cmds =
    List.map
      (fun (p : pin) -> Printf.sprintf "opam pin add -yn %s %s" p.package p.url)
      extra_pins
  in
  let install_cmd =
    "opam install -y js_of_ocaml js_top_worker-bin js_top_worker-web \
     js_top_worker-widget"
  in
  let verify_cmd = "eval $(opam env) && which js_of_ocaml && which jtw" in
  let jtw_cmd =
    "eval $(opam env) && jtw opam --no-worker -o /home/opam/jtw-tools-output \
     stdlib"
  in
  String.concat " && "
    (jtw_pin_cmds @ extra_pin_cmds @ [ install_cmd; verify_cmd; jtw_cmd ])

let build_cmd_local ~container_path ~extra_pins =
  let pin_cmds =
    List.map
      (fun pkg -> Printf.sprintf "opam pin add -yn %s %s" pkg container_path)
      jtw_packages
  in
  let extra_pin_cmds =
    List.map
      (fun (p : pin) -> Printf.sprintf "opam pin add -yn %s %s" p.package p.url)
      extra_pins
  in
  let install_cmd =
    "opam install -y js_of_ocaml js_top_worker-bin js_top_worker-web \
     js_top_worker-widget"
  in
  let verify_cmd = "eval $(opam env) && which js_of_ocaml && which jtw" in
  let jtw_cmd =
    "eval $(opam env) && jtw opam --no-worker -o /home/opam/jtw-tools-output \
     stdlib"
  in
  String.concat " && "
    (pin_cmds @ extra_pin_cmds @ [ install_cmd; verify_cmd; jtw_cmd ])

let tool_target = "js_top_worker-bin"
let extra_tool_targets = [ "js_top_worker-web" ]

let exists ~layer_dir =
  Bos.OS.File.exists Fpath.(layer_dir / "layer.json") |> Result.get_ok

let has_jsoo ~layer_dir =
  let path =
    Fpath.(
      layer_dir
      / "fs"
      / "home"
      / "opam"
      / ".opam"
      / "default"
      / "bin"
      / "js_of_ocaml")
  in
  Sys.file_exists (Fpath.to_string path)

let has_worker_js ~layer_dir =
  let path =
    Fpath.(
      layer_dir / "fs" / "home" / "opam" / "jtw-tools-output" / "worker.js")
  in
  Sys.file_exists (Fpath.to_string path)
