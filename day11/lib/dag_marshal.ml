type kind = Build | Tool | Compile | Doc_all | Link

type entry = {
  hash : string;
  pkg : OpamPackage.t;
  kind : kind;
  deps : string list;
  universe : string;
      (** Real output universe of this node — [compute_universe_hash] of the
          node's build-layer hash, i.e. the [u/<universe>/...] path the docs
          land in. ["" ] for nodes with no meaningful universe (tools) or old
          dag.json files written before this field existed. *)
  blessed : bool;
      (** Whether this node's universe is the blessed one for its package (the
          per-universe blessing decision, not package-level). [false] for nodes
          from dag.json files predating this field. *)
}

let kind_to_string = function
  | Build -> "build"
  | Tool -> "tool"
  | Compile -> "compile"
  | Doc_all -> "doc_all"
  | Link -> "link"

let kind_of_string = function
  | "build" -> Some Build
  | "tool" -> Some Tool
  | "compile" -> Some Compile
  | "doc_all" -> Some Doc_all
  | "link" -> Some Link
  | _ -> None

let path snapshot_dir = Fpath.(snapshot_dir / "dag.json")

let entry_to_json (e : entry) : Yojson.Safe.t =
  `Assoc
    [
      ("hash", `String e.hash);
      ("pkg", `String (OpamPackage.to_string e.pkg));
      ("kind", `String (kind_to_string e.kind));
      ("deps", `List (List.map (fun h -> `String h) e.deps));
      ("universe", `String e.universe);
      ("blessed", `Bool e.blessed);
    ]

let to_json entries : Yojson.Safe.t =
  `Assoc
    [ ("version", `Int 1); ("nodes", `List (List.map entry_to_json entries)) ]

let write ~snapshot_dir entries =
  let p = path snapshot_dir in
  match Bos.OS.Dir.create ~path:true snapshot_dir with
  | Error _ as e -> e
  | Ok _ -> Bos.OS.File.write p (Yojson.Safe.to_string (to_json entries))

let entry_of_json (j : Yojson.Safe.t) =
  let open Yojson.Safe.Util in
  try
    let hash = j |> member "hash" |> to_string in
    let pkg_s = j |> member "pkg" |> to_string in
    let pkg = OpamPackage.of_string pkg_s in
    let kind_s = j |> member "kind" |> to_string in
    let kind =
      match kind_of_string kind_s with
      | Some k -> k
      | None -> failwith (Printf.sprintf "unknown kind %S" kind_s)
    in
    let deps = j |> member "deps" |> to_list |> List.map to_string in
    let universe =
      match member "universe" j with
      | `String s -> s
      | _ -> ""
      | exception _ -> ""
    in
    let blessed =
      match member "blessed" j with
      | `Bool b -> b
      | _ -> false
      | exception _ -> false
    in
    Ok { hash; pkg; kind; deps; universe; blessed }
  with exn ->
    Rresult.R.error_msgf "Dag_marshal.entry_of_json: %s"
      (Printexc.to_string exn)

let read ~snapshot_dir =
  let p = path snapshot_dir in
  match Bos.OS.File.read p with
  | Error _ as e -> e
  | Ok data -> (
      try
        let json = Yojson.Safe.from_string data in
        let nodes_j =
          json |> Yojson.Safe.Util.member "nodes" |> Yojson.Safe.Util.to_list
        in
        let rec collect = function
          | [] -> Ok []
          | x :: xs -> (
              match entry_of_json x with
              | Error e -> Error e
              | Ok e -> (
                  match collect xs with
                  | Error err -> Error err
                  | Ok rest -> Ok (e :: rest)))
        in
        collect nodes_j
      with exn ->
        Rresult.R.error_msgf "Dag_marshal.read: %s" (Printexc.to_string exn))
