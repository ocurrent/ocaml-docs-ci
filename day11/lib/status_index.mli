(** Global status index.

    Aggregates the plan's per-node build/doc outcomes into blessed /
    non-blessed category totals, written to [status.json] for the web
    dashboard. Computed from outcomes supplied by the caller (the daemon
    pipeline or the [day11 batch] CLI) — cache hits included — so it
    reflects the full plan state, not just what this run re-dispatched. *)

(** A complete status snapshot for one run. *)
type t = {
  generated : string;                   (** ISO-8601 generation timestamp. *)
  run_id : string;                      (** Unique run identifier. *)
  scanned : int;                        (** Packages the plan covered. *)
  blessed_totals : (string * int) list;     (** Category counts for blessed builds. *)
  non_blessed_totals : (string * int) list; (** Category counts for non-blessed builds. *)
}

(** One planned node's outcome — the cheapest common value both
    producers of [status.json] have to hand (see {!of_outcomes}). *)
type node_outcome = {
  is_doc : bool;   (** Doc node (compile/doc-all/link) vs build/tool. *)
  blessed : bool;  (** The plan's per-node blessing. *)
  ok : bool;       (** Build succeeded (cache hits count as [true]). *)
  cascaded : bool; (** Only meaningful when [not ok]: the node never ran
                       because a dependency failed (a cascade), as
                       opposed to failing on its own. Counted as
                       [dependency_failure]. *)
}

(** [of_outcomes ~run_id ~scanned outcomes] aggregates per-node outcomes
    into blessed / non-blessed category totals. Pure — reads nothing
    from disk and does not depend on run-id matching. *)
val of_outcomes : run_id:string -> scanned:int -> node_outcome list -> t

(** [final_status_of_outcomes items] collapses each blessed package's
    nodes (keyed by ["name.version"]) to a single status string
    (["doc_success"] / ["doc_failure"] / ["build_failure"] /
    ["dependency_failure"] / ["success"]), worst-outcome-first. Only
    blessed packages are included. *)
val final_status_of_outcomes :
  (string * node_outcome) list -> (string * string) list

(** Write the blessed-package [(name.version -> status)] table as
    [final_status.json] in [dir], for the snapshot diff views. *)
val write_final_status : dir:Fpath.t -> (string * string) list -> unit

(** Write the status index as [status.json] in [dir]. *)
val write : dir:Fpath.t -> t -> unit

(** Read a previously written status index from [dir], or [None]. *)
val read : dir:Fpath.t -> t option

(** Serialize a status index to JSON. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize a status index from JSON, returning [None] on malformed input. *)
val of_json : Yojson.Safe.t -> t option
