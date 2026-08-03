(** Run lifecycle and structured logging.

    Tracks the start/end of a batch run, collects build/doc logs, and produces a
    summary. *)

type t
(** Opaque run metadata. *)

type summary = {
  run_id : string;
  start_time : float;
  end_time : float;
  duration : float;
  targets_requested : int;
  packages_built : int;
  packages_failed : int;
  docs_generated : int;
  failures : (string * string) list;  (** (pkg, category) *)
}

val set_log_base_dir : string -> unit
val start_run : unit -> t
val get_id : t -> string
val get_run_dir : t -> string
val get_start_time : t -> float
val format_time : float -> string
val add_build_log : t -> package:string -> source_log:string -> unit

val add_doc_log :
  t -> package:string -> source_log:string -> layer_hash:string -> unit

val finish_run :
  t ->
  targets_requested:int ->
  packages_built:int ->
  packages_failed:int ->
  docs_generated:int ->
  failures:(string * string) list ->
  summary
(** {2 Incremental phase files}

    Written as the build progresses so status can be checked mid-run. *)

val write_plan :
  t ->
  repos_with_shas:(string * string) list ->
  n_targets:int ->
  ocaml_version:string option ->
  with_doc:bool ->
  all_versions:bool ->
  small_universe:bool ->
  unit

val write_solve : t -> n_solved:int -> n_failed:int -> unit
val write_dag : t -> n_build:int -> n_cached:int -> n_need_build:int -> unit

val write_doc_dag :
  t ->
  n_build:int ->
  n_tool:int ->
  n_doc_all:int ->
  n_compile:int ->
  n_link:int ->
  unit

val log_build_result :
  t ->
  pkg:string ->
  hash:string ->
  status:string ->
  failed_dep:string option ->
  ?kind:string ->
  ?layer_dir:string ->
  unit ->
  unit
(** Append one line to [build.jsonl]. Thread-safe. [kind] is ["build"],
    ["tool"], ["doc-all"], ["compile"], or ["link"]. [layer_dir] is the path to
    the layer on disk. *)

val write_dag_structure : t -> Day11_opam_layer.Build.t list -> unit
(** Write one JSONL line per node: hash, package, dep hashes. Enables offline
    analysis of DAG parallelism. *)

val close_build_log : unit -> unit
val summary_to_json : summary -> Yojson.Safe.t
