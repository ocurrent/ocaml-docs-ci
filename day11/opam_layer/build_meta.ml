(* Wire type: tolerates both the legacy [deps : string list] form and
   the new [deps : (pkg, hash) list] form for backward compatibility
   when reading. The on-disk JSON is now an array of objects:
     [{ "pkg": "fmt.0.9.0", "hash": "abc…" }, …]
   Old files where deps was a plain string array still load — each
   entry's hash is left empty and callers cross-reference
   [layer.json.parent_hashes] (which is parallel-ordered). *)
type dep = { pkg : string; hash : string [@default ""] }
[@@deriving yojson { strict = false }]

let dep_list_to_yojson (deps : dep list) : Yojson.Safe.t =
  `List (List.map dep_to_yojson deps)

let dep_list_of_yojson (json : Yojson.Safe.t) : (dep list, string) result =
  match json with
  | `List items ->
      List.fold_left
        (fun acc item ->
          match acc with
          | Error _ as e -> e
          | Ok xs -> (
              match item with
              | `String pkg -> Ok (xs @ [ { pkg; hash = "" } ])
              | `Assoc _ -> (
                  match dep_of_yojson item with
                  | Ok d -> Ok (xs @ [ d ])
                  | Error msg -> Error msg)
              | _ -> Error "deps: expected string or object"))
        (Ok []) items
  | _ -> Error "deps: expected array"

type t = {
  package : string;
  deps : dep list;
      [@default []]
      [@to_yojson dep_list_to_yojson]
      [@of_yojson dep_list_of_yojson]
  stack : string list; [@default []]
  build_deps : string list; [@default []]
  installed_libs : string list; [@default []]
  installed_docs : string list; [@default []]
  patches : string list; [@default []]
  base_image : string; [@default ""]
  cmd : string; [@default ""]
  universe : string; [@default ""]
}
[@@deriving yojson { strict = false }]

let filename = "build.json"

let save layer_dir t =
  let path = Fpath.(layer_dir / "build.json") in
  try
    Yojson.Safe.to_file (Fpath.to_string path) (to_yojson t);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Build_meta.save %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let load layer_dir =
  let path = Fpath.(layer_dir / "build.json") in
  try
    match of_yojson (Yojson.Safe.from_file (Fpath.to_string path)) with
    | Ok t -> Ok t
    | Error msg ->
        Rresult.R.error_msgf "Build_meta.load %a: %s" Fpath.pp path msg
  with exn ->
    Rresult.R.error_msgf "Build_meta.load %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let exists layer_dir =
  Bos.OS.File.exists Fpath.(layer_dir / "build.json")
  |> Result.value ~default:false

let load_tree env ~os_dir hash =
  let cache : (string, Build.t) Hashtbl.t = Hashtbl.create 16 in
  let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e in
  let rec load_h h =
    match Hashtbl.find_opt cache h with
    | Some b -> Ok b
    | None ->
        let layer = Day11_layer.Layer.of_hash ~os_dir h in
        let layer_dir = Day11_layer.Layer.dir layer in
        let* build_meta = load layer_dir in
        (* Source the dep hashes from [build.json] (which now carries
         [(pkg, hash)] pairs and is written pre-attempt — so failed
         layers have it). Fall back to [layer.json]'s
         [parent_hashes] for older layers whose [build.json] still
         has empty hash strings from the legacy [string list]
         format. *)
        let dep_hashes_from_bm =
          List.filter_map
            (fun (d : dep) -> if d.hash = "" then None else Some d.hash)
            build_meta.deps
        in
        let* dep_hashes =
          if List.length dep_hashes_from_bm = List.length build_meta.deps then
            Ok dep_hashes_from_bm
          else
            (* Legacy build.json without hashes — fall back to
             layer.json. Failed layers without layer.json end up
             with no walkable dep tree, but at least the root node
             loads, which is enough for [day11 debug]. *)
            match
              Day11_layer.Meta.load env (Day11_layer.Layer.meta_path layer)
            with
            | Ok lm -> Ok lm.parent_hashes
            | Error _ -> Ok []
        in
        let* deps = load_deps dep_hashes in
        let build : Build.t =
          {
            hash = h;
            pkg = OpamPackage.of_string build_meta.package;
            deps;
            universe = Day11_solution.Universe.dummy;
          }
        in
        Hashtbl.replace cache h build;
        Ok build
  and load_deps hashes =
    List.fold_left
      (fun acc h ->
        let* acc = acc in
        let* dep = load_h h in
        Ok (acc @ [ dep ]))
      (Ok []) hashes
  in
  load_h hash
