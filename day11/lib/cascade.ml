type status = Ok | Failed | Cascade of string | Pending
type result = { status : status; pkg : OpamPackage.t; kind : Dag_marshal.kind }

(** Build [pkg → (build_hash → entry)] from disk, scanning each unique package
    once. Latest entry per hash wins (matches {!History.read_latest}). *)
let load_history_index ~packages_dir entries =
  let by_pkg : (string, (string, History.entry) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 64
  in
  let load_pkg pkg_str =
    if Hashtbl.mem by_pkg pkg_str then ()
    else
      let entries = History.read_latest ~packages_dir ~pkg_str in
      let tbl = Hashtbl.create (List.length entries) in
      List.iter
        (fun (h : History.entry) ->
          if not (Hashtbl.mem tbl h.build_hash) then
            Hashtbl.add tbl h.build_hash h)
        entries;
      Hashtbl.add by_pkg pkg_str tbl
  in
  List.iter
    (fun (e : Dag_marshal.entry) -> load_pkg (OpamPackage.to_string e.pkg))
    entries;
  by_pkg

let lookup_history index pkg hash =
  let pkg_str = OpamPackage.to_string pkg in
  match Hashtbl.find_opt index pkg_str with
  | None -> None
  | Some tbl -> Hashtbl.find_opt tbl hash

(* A node's own on-disk status, before cascade attribution. [Unrun]
   means the node didn't run (no record / missing layer) — the shared
   walk then pins it to a failed dep ([Cascade]) or leaves it [Pending]. *)
type direct = D_ok | D_failed | D_unrun

(* Shared cascade classifier. Memoised DFS over the DAG: classify each
   node's deps first, then this node — [Ok]/[Failed] straight from
   [direct_status], or for an unrun node attribute to the first failed/
   cascaded dep. The two public entry points differ only in
   [direct_status] (per-package history vs. on-disk layer status). *)
let classify_dfs ~direct_status entries =
  let by_hash : (string, Dag_marshal.entry) Hashtbl.t =
    Hashtbl.create (List.length entries)
  in
  List.iter
    (fun (e : Dag_marshal.entry) -> Hashtbl.replace by_hash e.hash e)
    entries;
  let table : (string, result) Hashtbl.t =
    Hashtbl.create (List.length entries)
  in
  let visiting : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let rec classify_one h =
    match Hashtbl.find_opt table h with
    | Some r -> Some r
    | None -> (
        match Hashtbl.find_opt by_hash h with
        | None -> None
        | Some e ->
            if Hashtbl.mem visiting h then (
              (* DAG cycle — shouldn't happen, but stay safe. *)
              let r = { status = Pending; pkg = e.pkg; kind = e.kind } in
              Hashtbl.replace table h r;
              Some r)
            else (
              Hashtbl.add visiting h ();
              List.iter (fun dh -> ignore (classify_one dh)) e.deps;
              Hashtbl.remove visiting h;
              let status =
                match direct_status e h with
                | D_ok -> Ok
                | D_failed -> Failed
                | D_unrun -> (
                    match
                      List.find_map
                        (fun dh ->
                          match Hashtbl.find_opt table dh with
                          | Some { status = Failed; _ } -> Some dh
                          | Some { status = Cascade src; _ } -> Some src
                          | _ -> None)
                        e.deps
                    with
                    | Some src -> Cascade src
                    | None -> Pending)
              in
              let r = { status; pkg = e.pkg; kind = e.kind } in
              Hashtbl.replace table h r;
              Some r))
  in
  List.iter
    (fun (e : Dag_marshal.entry) -> ignore (classify_one e.hash))
    entries;
  table

let classify ~packages_dir entries =
  let history_index = load_history_index ~packages_dir entries in
  let direct_status (e : Dag_marshal.entry) h =
    match lookup_history history_index e.pkg h with
    | Some he when he.status = "success" -> D_ok
    | Some he when he.status = "failure" -> D_failed
    | Some _ | None -> D_unrun
  in
  classify_dfs ~direct_status entries

let root_cause table hash =
  match Hashtbl.find_opt table hash with
  | Some { status = Failed; _ } -> Some hash
  | Some { status = Cascade src; _ } -> Some src
  | _ -> None

let classify_from_layer_index ~status_index entries =
  let direct_status (_ : Dag_marshal.entry) h =
    let key = String.sub h 0 (min 12 (String.length h)) in
    match Hashtbl.find_opt status_index key with
    | None -> D_unrun
    | Some (e : Day11_layer.Layer_status.entry) ->
        if e.exit_status = 0 then D_ok else D_failed
  in
  classify_dfs ~direct_status entries

let classify_from_layers ~os_dir entries =
  let status_index = Day11_layer.Layer_status.load ~os_dir in
  classify_from_layer_index ~status_index entries
