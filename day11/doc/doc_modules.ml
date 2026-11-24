type package = { name : string; version : string; modules : string list }

let default_max_depth = 3
let default_max_paths = 500
let filename = "doc-modules.jsonl.gz"
let format = "ocamlorg-doc-modules"
let format_version = 1

let is_opam_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '+' | '.' | '~' -> true
  | _ -> false

let is_opam_word s = s <> "" && String.for_all is_opam_char s

let is_module_name s =
  s <> ""
  && (match s.[0] with 'A' .. 'Z' -> true | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       s

exception Not_a_sidebar

(* Every module path of a sidebar forest, as reversed segment lists. *)
let rec paths prefix nodes acc =
  List.fold_left
    (fun acc node ->
      match node with
      | `Assoc fields ->
          let kind, content =
            match List.assoc_opt "node" fields with
            | Some (`Assoc n) ->
                (List.assoc_opt "kind" n, List.assoc_opt "content" n)
            | _ -> (None, None)
          in
          let children =
            match List.assoc_opt "children" fields with
            | Some (`List l) -> l
            | None | Some `Null -> []
            | Some _ -> raise Not_a_sidebar
          in
          let acc, prefix =
            match (kind, content) with
            | Some (`String ("module" | "module-type")), Some (`String c) ->
                let p = c :: prefix in
                (p :: acc, p)
            | Some (`String ("module" | "module-type")), _ ->
                raise Not_a_sidebar
            | _ -> (acc, prefix)
          in
          if kind = Some (`String "source") then acc
          else paths prefix children acc
      | _ -> raise Not_a_sidebar)
    acc nodes

let modules_of_sidebar ?(max_depth = default_max_depth)
    ?(max_paths = default_max_paths) json =
  if max_depth < 1 then invalid_arg "Doc_modules: max_depth < 1";
  if max_paths < 1 then invalid_arg "Doc_modules: max_paths < 1";
  match json with
  | `List (_ :: _ as nodes) -> (
      match paths [] nodes [] with
      | exception Not_a_sidebar -> None
      | all ->
          let kept =
            List.filter_map
              (fun rev ->
                if
                  List.length rev <= max_depth
                  && List.for_all is_module_name rev
                then Some (List.length rev, String.concat "." (List.rev rev))
                else None)
              all
            |> List.sort_uniq compare
          in
          Some (List.filteri (fun i _ -> i < max_paths) kept |> List.map snd))
  | _ -> None

let readdir dir = try Sys.readdir dir |> Array.to_list with Sys_error _ -> []

let sidebar_modules ?max_depth ?max_paths file =
  match Yojson.Safe.from_file file with
  | exception (Sys_error _ | Yojson.Json_error _) -> None
  | json -> (
      match modules_of_sidebar ?max_depth ?max_paths json with
      | Some (_ :: _ as modules) -> Some modules
      | Some [] | None -> None)

let collect ?max_depth ?max_paths html_dir =
  let p_dir = Fpath.(to_string (html_dir / "p")) in
  readdir p_dir
  |> List.filter is_opam_word
  |> List.sort String.compare
  |> List.filter_map (fun name ->
      let pkg_dir = Filename.concat p_dir name in
      let versions =
        readdir pkg_dir
        |> List.filter is_opam_word
        |> List.sort (fun a b ->
            OpamPackage.Version.(compare (of_string b) (of_string a)))
      in
      List.find_map
        (fun version ->
          let sidebar =
            String.concat Filename.dir_sep
              [ pkg_dir; version; "doc"; "sidebar.json" ]
          in
          Option.map
            (fun modules -> { name; version; modules })
            (sidebar_modules ?max_depth ?max_paths sidebar))
        versions)

let to_jsonl packages =
  let b = Buffer.create 65536 in
  let line json =
    Buffer.add_string b (Yojson.Safe.to_string json);
    Buffer.add_char b '\n'
  in
  line (`Assoc [ ("format", `String format); ("version", `Int format_version) ]);
  List.iter
    (fun p ->
      line
        (`Assoc
           [
             ("name", `String p.name);
             ("version", `String p.version);
             ("modules", `List (List.map (fun m -> `String m) p.modules));
           ]))
    (List.sort (fun a b -> String.compare a.name b.name) packages);
  Buffer.contents b

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | s -> Some s
  | exception Sys_error _ -> None

let write ?max_depth ?max_paths html_dir =
  let packages = collect ?max_depth ?max_paths html_dir in
  let target = Fpath.add_seg html_dir filename in
  let tmp = Fpath.add_seg html_dir (filename ^ ".tmp") in
  let gzip = Bos.Cmd.(v "gzip" % "-n" % "-9" % "-c") in
  let result =
    match
      Bos.OS.Cmd.(in_string (to_jsonl packages) |> run_io gzip |> to_file tmp)
    with
    | Error (`Msg e) -> Error e
    | Ok () -> (
        match
          (read_file (Fpath.to_string tmp), read_file (Fpath.to_string target))
        with
        | Some fresh, Some current when String.equal fresh current ->
            Ok `Unchanged
        | Some _, _ -> (
            match
              Unix.rename (Fpath.to_string tmp) (Fpath.to_string target)
            with
            | () -> Ok `Written
            | exception Unix.Unix_error (e, _, _) ->
                Error ("rename: " ^ Unix.error_message e))
        | None, _ -> Error ("cannot read " ^ Fpath.to_string tmp))
  in
  ignore (Bos.OS.File.delete tmp);
  Result.map (fun w -> (w, List.length packages)) result
