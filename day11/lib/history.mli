(** Per-package build history (append-only JSONL).

    Each package has a [history.jsonl] file with one JSON entry per build. Uses
    file locking for concurrent access safety. *)

type entry = {
  ts : string;  (** ISO-8601 timestamp of the build. *)
  run : string;  (** Run identifier this entry belongs to. *)
  build_hash : string;  (** Content hash of the build layer. *)
  status : string;  (** Outcome status (e.g. ["ok"], ["fail"]). *)
  category : string;  (** Failure category (e.g. ["build_failure"]). *)
  blessed : bool;  (** Whether this is the blessed (primary) build. *)
  error : string option;  (** Optional error message. *)
  universe : string;
      (** Output universe of the node (doc nodes only); ["" ] for build/tool
          nodes and legacy entries. *)
}
(** A single build history entry recording what happened and when. Fields like
    compiler/dependency are intentionally absent — they are derivable from the
    build_hash via the per-snapshot [dag.json], which avoids storing the same
    fact twice. *)

val append : packages_dir:Fpath.t -> pkg_str:string -> entry -> unit
(** Append an entry to the history file for [pkg_str] under [packages_dir].
    Creates the file if it does not exist. *)

val read : packages_dir:Fpath.t -> pkg_str:string -> entry list
(** Read all history entries for [pkg_str], most recent first. *)

val read_latest : packages_dir:Fpath.t -> pkg_str:string -> entry list
(** Read the latest entry per unique {!field:entry.build_hash}, most recent
    first. *)

val read_blessed : packages_dir:Fpath.t -> pkg_str:string -> entry option
(** Return the most recent blessed entry, or [None]. *)

val compact : packages_dir:Fpath.t -> pkg_str:string -> max_age_days:int -> unit
(** Remove duplicate consecutive entries older than [max_age_days], keeping the
    first and last of each run of identical results. *)

val entry_to_json : entry -> Yojson.Safe.t
(** Serialize an entry to JSON. *)

val entry_of_json : Yojson.Safe.t -> entry option
(** Deserialize an entry from JSON, returning [None] on malformed input. *)
