(* Driver layer *)

let driver_layer_hash ~base_hash ~compiler_hashes =
  Day11_layer.Hash.of_strings ([ "driver"; base_hash ] @ compiler_hashes)

let driver_layer_name ~base_hash ~compiler_hashes =
  let hash = driver_layer_hash ~base_hash ~compiler_hashes in
  "doc-driver-" ^ String.sub hash 0 12

let driver_build_script ~packages ~pin_commands =
  let pins = String.concat "\n" pin_commands in
  let install =
    Printf.sprintf "opam install -y %s" (String.concat " " packages)
  in
  String.concat "\n" ((if pins = "" then [] else [ pins ]) @ [ install ])

let driver_exists env ~layer_dir =
  Day11_layer.Layer.exists env { hash = ""; dir = layer_dir }

let has_odoc_driver_voodoo ~layer_dir =
  let bin =
    Fpath.(
      layer_dir
      / "fs"
      / "home"
      / "opam"
      / ".opam"
      / "default"
      / "bin"
      / "odoc_driver_voodoo")
  in
  Sys.file_exists (Fpath.to_string bin)

(* Odoc layer *)

let odoc_layer_hash ~base_hash ~ocaml_version ~compiler_hashes =
  Day11_layer.Hash.of_strings
    ([ "odoc"; base_hash; ocaml_version ] @ compiler_hashes)

let odoc_layer_name ~base_hash ~ocaml_version ~compiler_hashes =
  let hash = odoc_layer_hash ~base_hash ~ocaml_version ~compiler_hashes in
  "doc-odoc-" ^ String.sub hash 0 12

let odoc_build_script ~packages ~pin_commands =
  driver_build_script ~packages ~pin_commands

let odoc_exists env ~layer_dir = driver_exists env ~layer_dir

let has_odoc ~layer_dir =
  let bin =
    Fpath.(
      layer_dir / "fs" / "home" / "opam" / ".opam" / "default" / "bin" / "odoc")
  in
  Sys.file_exists (Fpath.to_string bin)
