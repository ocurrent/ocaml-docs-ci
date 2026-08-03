let compute_layer_hash ~build_hash ~tools_hash =
  Day11_layer.Hash.of_strings [ "jtw"; build_hash; tools_hash ]

let collect_files_sorted ~base ~pred =
  let files = ref [] in
  let rec walk rel =
    let full = if rel = "" then base else Filename.concat base rel in
    if Sys.file_exists full && Sys.is_directory full then
      let entries = try Sys.readdir full |> Array.to_list with _ -> [] in
      let entries = List.sort String.compare entries in
      List.iter
        (fun name ->
          let sub = if rel = "" then name else rel ^ "/" ^ name in
          walk sub)
        entries
    else if pred rel then files := rel :: !files
  in
  walk "";
  List.rev !files

let compute_content_hash lib_dir =
  let is_payload f =
    Filename.check_suffix f ".cmi"
    || Filename.check_suffix f ".cma.js"
    || Filename.basename f = "META"
  in
  let files = collect_files_sorted ~base:lib_dir ~pred:is_payload in
  let buf = Buffer.create 4096 in
  List.iter
    (fun rel ->
      Buffer.add_string buf rel;
      Buffer.add_char buf '\000';
      let content =
        In_channel.with_open_bin
          (Filename.concat lib_dir rel)
          In_channel.input_all
      in
      Buffer.add_string buf content;
      Buffer.add_char buf '\000')
    files;
  let hash = Digest.to_hex (Digest.string (Buffer.contents buf)) in
  String.sub hash 0 16

let compute_compiler_content_hash tools_output_dir =
  let buf = Buffer.create 4096 in
  let worker_path = Filename.concat tools_output_dir "worker.js" in
  if Sys.file_exists worker_path then (
    Buffer.add_string buf "worker.js";
    Buffer.add_char buf '\000';
    Buffer.add_string buf
      (In_channel.with_open_bin worker_path In_channel.input_all);
    Buffer.add_char buf '\000');
  let lib_dir = Filename.concat tools_output_dir "lib" in
  (if Sys.file_exists lib_dir then
     let is_cmi f = Filename.check_suffix f ".cmi" in
     let files = collect_files_sorted ~base:lib_dir ~pred:is_cmi in
     List.iter
       (fun rel ->
         Buffer.add_string buf ("lib/" ^ rel);
         Buffer.add_char buf '\000';
         Buffer.add_string buf
           (In_channel.with_open_bin
              (Filename.concat lib_dir rel)
              In_channel.input_all);
         Buffer.add_char buf '\000')
       files);
  let hash = Digest.to_hex (Digest.string (Buffer.contents buf)) in
  String.sub hash 0 16

let generate_dynamic_cmis_json ~dcs_url cmi_filenames =
  let all_cmis =
    List.map
      (fun s ->
        if Filename.check_suffix s ".cmi" then
          String.sub s 0 (String.length s - 4)
        else s)
      cmi_filenames
  in
  let hidden, non_hidden =
    List.partition
      (fun x ->
        try
          let _ = Str.search_forward (Str.regexp_string "__") x 0 in
          true
        with Not_found -> false)
      all_cmis
  in
  let prefixes =
    List.filter_map
      (fun x ->
        try
          let pos = Str.search_forward (Str.regexp_string "__") x 0 in
          Some (String.sub x 0 (pos + 2))
        with Not_found -> None)
      hidden
  in
  let prefixes = List.sort_uniq String.compare prefixes in
  let toplevel_modules =
    List.map String.capitalize_ascii non_hidden |> List.sort String.compare
  in
  let json_list xs =
    "[" ^ String.concat "," (List.map (fun s -> Printf.sprintf "%S" s) xs) ^ "]"
  in
  Printf.sprintf
    {|{"dcs_url":%S,"dcs_toplevel_modules":%s,"dcs_file_prefixes":%s}|} dcs_url
    (json_list toplevel_modules)
    (json_list prefixes)

let generate_findlib_index ~compiler meta_paths =
  let metas = List.map (fun p -> `String p) meta_paths in
  Yojson.Safe.to_string
    (`Assoc [ ("compiler", compiler); ("meta_files", `List metas) ])

let findlib_names_of_installed_libs installed_libs =
  List.filter_map
    (fun rel_path ->
      if Filename.basename rel_path = "META" then
        match String.split_on_char '/' (Filename.dirname rel_path) with
        | top :: _ -> Some top
        | [] -> None
      else None)
    installed_libs
  |> List.sort_uniq String.compare

let container_script ~pkg ~installed_libs =
  let pkg_name = OpamPackage.name_to_string pkg in
  let findlib_names = findlib_names_of_installed_libs installed_libs in
  if findlib_names = [] then "true"
  else
    let libs = String.concat " " (List.map Filename.quote findlib_names) in
    String.concat " && "
      [
        "eval $(opam env)";
        Printf.sprintf
          "echo 'JTW: Building %s via jtw opam (%d findlib packages)'" pkg_name
          (List.length findlib_names);
        Printf.sprintf
          "jtw opam --path %s --no-worker -o /home/opam/jtw-output %s"
          (Filename.quote pkg_name) libs;
        "echo 'JTW: Done'";
      ]

type jtw_result = Jtw_success | Jtw_failure of string | Jtw_skipped

let jtw_result_to_yojson = function
  | Jtw_success -> `Assoc [ ("status", `String "success") ]
  | Jtw_failure msg ->
      `Assoc [ ("status", `String "failure"); ("error", `String msg) ]
  | Jtw_skipped -> `Assoc [ ("status", `String "skipped") ]
