(** Live view of an in-progress run.

    Reads [runs/<latest>/build.jsonl] and (when present) the phase JSON files
    written by {!Day11_lib.Run_log} to produce a structured snapshot of progress
    so far. Lets CLIs show meaningful output before [summary.json] /
    [status.json] are written at run end. *)

type counts = { ok : int; fail : int; cascade : int }

type progress = {
  completed : int;
  total : int option;  (** [None] when [doc_dag.json] is absent. *)
  by_kind : (string * int) list;
      (** [(kind, completed_count)] for each kind seen. *)
  kind_totals : (string * int) list;
      (** [(kind, total_planned)] from [doc_dag.json]. Empty if absent. *)
}

type t = {
  run_dir : Fpath.t;
  run_id : string;  (** Directory name (e.g. "2026-04-22-112301"). *)
  pkg_status : (string, string) Hashtbl.t;
      (** [pkg_str -> "ok" | "fail" | "cascade"] — "ok" wins if a package has
          multiple hashes. *)
  failed_dep : (string, string) Hashtbl.t;
      (** [pkg_str -> failed_dep_pkg_str] for cascade entries. *)
  counts : counts;
  progress : progress;
}

val load_latest : snapshot_dir:Fpath.t -> t option
(** Load the most recent run under [snapshot_dir/runs/]. Returns [None] if the
    snapshot has no runs or [build.jsonl] is missing. *)

val is_terminal : snapshot_dir:Fpath.t -> bool
(** [true] iff [status.json] exists — i.e. the pipeline has written a finished
    state and callers can use the usual code path. *)

val format_progress : progress -> string
(** One-line summary, e.g. [" 128/4430 (2.9%) — build=120 tool=2 compile=6"]. *)
