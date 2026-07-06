(** Result aggregation and reporting.

    Collects build and doc outcomes, records them in per-package
    history files, generates the status index, and prints a
    human-readable summary. *)

type build_outcome = {
  pkg : OpamPackage.t;
  build_hash : string;
  success : bool;
  log_file : Fpath.t option;
  blessed : bool;
}

type doc_outcome = {
  pkg : OpamPackage.t;
  success : bool;
  layer_hash : string;
  log_file : Fpath.t option;
  blessed : bool;
}

type results = {
  builds : build_outcome list;
  docs : doc_outcome list;
  targets : OpamPackage.t list;
}

val generate_status :
  snapshot_dir:Fpath.t ->
  packages_dir:Fpath.t ->
  run_id:string ->
  Day11_lib.Status_index.t
(** Regenerate [status.json] from current on-disk history, detecting
    changes from the previous snapshot, and return the snapshot. Safe to
    call from anywhere (e.g. a periodic timer) — it just reads
    [packages/*/history.jsonl] and writes [snapshot_dir/status.json]. *)

val finish :
  snapshot_dir:Fpath.t ->
  packages_dir:Fpath.t ->
  run_info:Day11_lib.Run_log.t ->
  results ->
  Day11_lib.Run_log.summary
(** [finish ~snapshot_dir ~packages_dir ~run_info results] generates
    status.json from the (already incrementally-written) history,
    finishes the run log, and prints a summary to stdout. Returns
    the run summary. *)
