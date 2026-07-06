(** Global status index.

    Aggregates all packages' current build status into a single
    snapshot, detecting changes from the previous snapshot. Written
    to [status.json] for consumption by the web dashboard and
    notification system. *)

(** A status change for a single package between two runs. *)
type change = {
  package : string;         (** Package name (e.g. ["foo.1.0"]). *)
  build_hash : string;      (** Content hash of the build layer. *)
  blessed : bool;           (** Whether this is the blessed build. *)
  from_status : string;     (** Previous status category. *)
  to_status : string;       (** New status category. *)
}

(** A complete status snapshot for one run. *)
type t = {
  generated : string;                   (** ISO-8601 generation timestamp. *)
  run_id : string;                      (** Unique run identifier. *)
  scanned : int;                        (** Package directories scanned. *)
  blessed_totals : (string * int) list;     (** Category counts for blessed builds. *)
  non_blessed_totals : (string * int) list; (** Category counts for non-blessed builds. *)
  changes : change list;                (** Status changes since the previous run. *)
  new_packages : string list;           (** Packages seen for the first time in this run. *)
}

(** Scan [packages_dir] and build a status snapshot for [run_id],
    computing changes relative to [previous] (if provided). *)
val generate : packages_dir:Fpath.t -> run_id:string ->
  previous:t option -> t

(** Write the status index as [status.json] in [dir]. *)
val write : dir:Fpath.t -> t -> unit

(** Read a previously written status index from [dir], or [None]. *)
val read : dir:Fpath.t -> t option

(** Serialize a status index to JSON. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize a status index from JSON, returning [None] on malformed input. *)
val of_json : Yojson.Safe.t -> t option
