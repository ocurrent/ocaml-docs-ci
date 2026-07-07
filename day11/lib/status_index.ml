let src = Logs.Src.create "day11.status_index"
    ~doc:"Status index (status.json) read/write"
module Log = (val Logs.src_log src)

type t = {
  generated : string;
  run_id : string;
  scanned : int;
  blessed_totals : (string * int) list;
  non_blessed_totals : (string * int) list;
}

(* One planned node's outcome, the cheapest common value both producers
   of [status.json] have to hand: the daemon pipeline from the collapsed
   build results joined with the plan, and the [day11 batch] CLI from its
   per-node build/doc outcomes. [is_doc] distinguishes doc nodes
   (compile/doc-all/link) from build/tool; [blessed] is the plan's
   per-node blessing; [ok] is build success (cache hits count as ok). *)
type node_outcome = {
  is_doc : bool;
  blessed : bool;
  ok : bool;
  cascaded : bool;  (* only meaningful when [not ok]: the node never ran
                       because a dependency failed, vs. failing itself. *)
}

let totals_to_json (t : (string * int) list) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) t)

let totals_of_json (json : Yojson.Safe.t) : (string * int) list =
  match json with
  | `Assoc assoc ->
    List.filter_map (fun (k, v) ->
      match v with
      | `Int n -> Some (k, n)
      | _ -> None
    ) assoc
  | _ -> []

let to_json (t : t) : Yojson.Safe.t =
  `Assoc [
    ("generated", `String t.generated);
    ("run_id", `String t.run_id);
    ("scanned", `Int t.scanned);
    ("blessed_totals", totals_to_json t.blessed_totals);
    ("non_blessed_totals", totals_to_json t.non_blessed_totals);
  ]

let of_json (json : Yojson.Safe.t) : t option =
  match json with
  | `Assoc assoc ->
    let s key =
      match List.assoc_opt key assoc with
      | Some (`String s) -> Some s
      | _ -> None
    in
    (match s "generated", s "run_id" with
     | Some generated, Some run_id ->
       let scanned =
         match List.assoc_opt "scanned" assoc with
         | Some (`Int n) -> n | _ -> 0
       in
       (* Legacy [changes_since_last] / [new_packages] fields (if present
          in an older status.json) are ignored — no longer tracked. *)
       Some {
         generated;
         run_id;
         scanned;
         blessed_totals = totals_of_json
           (match List.assoc_opt "blessed_totals" assoc with
            | Some j -> j | None -> `Assoc []);
         non_blessed_totals = totals_of_json
           (match List.assoc_opt "non_blessed_totals" assoc with
            | Some j -> j | None -> `Assoc []);
       }
     | _ -> None)
  | _ -> None

let iso8601_now () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let incr_totals totals category =
  match List.assoc_opt category totals with
  | Some n -> (category, n + 1) :: List.filter (fun (k, _) -> k <> category) totals
  | None -> (category, 1) :: totals

let category ~is_doc ~ok ~cascaded =
  if ok then (if is_doc then "doc_success" else "success")
  (* A cascade (a dep failed, so this node never built) is distinct from a
     node that ran and failed on its own, but still stays on its own side
     of the build/doc split — otherwise a doc node that cascaded would be
     miscounted as a build. *)
  else if cascaded then
    (if is_doc then "doc_dependency_failure" else "dependency_failure")
  else if is_doc then "doc_failure"
  else "build_failure"

(* Aggregate per-node outcomes into blessed / non-blessed category
   totals. This is the whole computation now: it reads nothing from disk
   and does not depend on any run id matching — the caller (daemon or
   CLI) supplies the plan's outcomes directly, cache hits included, so
   the counts reflect the full plan state rather than only what this run
   happened to (re)dispatch. [scanned] is the number of packages the
   plan covered, passed by the caller. *)
let of_outcomes ~run_id ~scanned (outcomes : node_outcome list) : t =
  let blessed_totals = ref [] and non_blessed_totals = ref [] in
  List.iter (fun o ->
    let cat = category ~is_doc:o.is_doc ~ok:o.ok ~cascaded:o.cascaded in
    if o.blessed then blessed_totals := incr_totals !blessed_totals cat
    else non_blessed_totals := incr_totals !non_blessed_totals cat
  ) outcomes;
  let sum = List.fold_left (fun acc (_, n) -> acc + n) 0 in
  (* App level so it shows in the daemon log regardless of verbosity. *)
  Log.app (fun f -> f "generated status (run %s): %d packages, \
    blessed=%d non_blessed=%d"
    run_id scanned (sum !blessed_totals) (sum !non_blessed_totals));
  {
    generated = iso8601_now ();
    run_id;
    scanned;
    blessed_totals = !blessed_totals;
    non_blessed_totals = !non_blessed_totals;
  }

(* Per-blessed-package status for the snapshot diff views. Collapses a
   package's blessed nodes (its canonical universe) to one status,
   worst-first: a cascade (dep failed) dominates a build failure, which
   dominates a doc failure, else all built. Only blessed packages are
   emitted — the canonical universe is what the diffs compare. *)
let final_status_of_outcomes (items : (string * node_outcome) list)
  : (string * string) list =
  let by_pkg : (string, node_outcome list) Hashtbl.t = Hashtbl.create 4096 in
  List.iter (fun (pkg, o) ->
    if o.blessed then
      let prev = try Hashtbl.find by_pkg pkg with Not_found -> [] in
      Hashtbl.replace by_pkg pkg (o :: prev)
  ) items;
  Hashtbl.fold (fun pkg os acc ->
    let any f = List.exists f os in
    let status =
      if any (fun o -> (not o.ok) && o.cascaded) then "dependency_failure"
      else if any (fun o -> (not o.ok) && not o.is_doc) then "build_failure"
      else if any (fun o -> not o.ok) then "doc_failure"
      else if any (fun o -> o.is_doc) then "doc_success"
      else "success"
    in
    (pkg, status) :: acc
  ) by_pkg []

let final_status_path dir = Fpath.to_string Fpath.(dir / "final_status.json")

(* Atomic write of the [(name.version -> status)] table for blessed
   packages, keyed by [OpamPackage.to_string]. Written once the full
   plan's results are in, to drive the snapshot diff views. *)
let write_final_status ~dir (entries : (string * string) list) =
  let json = `Assoc (List.map (fun (k, v) -> (k, `String v)) entries) in
  let path = final_status_path dir in
  let tmp_path = path ^ ".tmp" in
  let oc = open_out tmp_path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc (Yojson.Safe.pretty_to_string json);
    output_char oc '\n');
  Sys.rename tmp_path path

let status_path dir = Fpath.to_string Fpath.(dir / "status.json")

let write ~dir t =
  let path = status_path dir in
  let json_str = Yojson.Safe.pretty_to_string (to_json t) in
  let tmp_path = path ^ ".tmp" in
  let oc = open_out tmp_path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc json_str;
    output_char oc '\n');
  Sys.rename tmp_path path

let read ~dir =
  let path = status_path dir in
  if not (Sys.file_exists path) then begin
    (* Normal early in a fresh run: status.json isn't written until the
       first [generate_status] pass completes. At info so "page shows
       no totals" is explainable from the log (file genuinely absent vs.
       a parse failure below). *)
    Log.info (fun f -> f "status.json not present yet: %s" path);
    None
  end
  else
    match Yojson.Safe.from_file path with
    | exception exn ->
      Log.warn (fun f -> f "status.json unreadable / invalid JSON (%s): %s"
        path (Printexc.to_string exn));
      None
    | json ->
      match of_json json with
      | Some _ as r -> r
      | None ->
        Log.warn (fun f -> f "status.json parsed as JSON but failed \
          structural validation (missing or non-string generated/run_id?): \
          %s" path);
        None
