(** Offline cascade attribution.

    Given a planned DAG ([<snapshot_dir>/dag.json] via {!Dag_marshal}) and the
    per-package history files, classify every node as one of:

    - [Ok]: ran and succeeded (history records [status=success])
    - [Failed]: ran and failed (history records [status=failure]) — a root cause
    - [Cascade source_hash]: did not run because a failed ancestor;
      [source_hash] points at the closest [Failed] node in the ancestry chain
    - [Pending]: did not run, and no failed ancestor explains why (the node is
      in-flight, or the run hasn't reached it yet)

    Cascades are derived, not stored. The recorder writes only "this node ran"
    entries; everything else is recovered here. *)

type status = Ok | Failed | Cascade of string | Pending
type result = { status : status; pkg : OpamPackage.t; kind : Dag_marshal.kind }

val classify :
  packages_dir:Fpath.t -> Dag_marshal.entry list -> (string, result) Hashtbl.t
(** [classify ~packages_dir entries] returns a hash → result table for every
    entry. Reads each unique package's [history.jsonl] once. *)

val classify_from_layers :
  os_dir:Fpath.t -> Dag_marshal.entry list -> (string, result) Hashtbl.t
(** [classify_from_layers ~os_dir entries] returns a comprehensive hash → result
    table derived from on-disk layer state — answering "what's the persistent
    state of this DAG", independent of which nodes happened to dispatch in any
    particular run.

    A layer is considered [Ok] iff the layer dir's [layer.json] exists and
    reports [exit_status = 0]; [Failed] if the layer dir exists with non-zero
    exit status; otherwise the node didn't run, and we walk its deps to find a
    [Failed]/[Cascade] ancestor (root cause) or report [Pending]. *)

val classify_from_layer_index :
  status_index:(string, Day11_layer.Layer_status.entry) Hashtbl.t ->
  Dag_marshal.entry list ->
  (string, result) Hashtbl.t
(** Variant of {!classify_from_layers} that takes a pre-loaded layer status
    index. Hot path for the dashboard, where one [layer_status.jsonl] is shared
    across many snapshot pages. *)

val root_cause : (string, result) Hashtbl.t -> string -> string option
(** Walk a cascade chain to its [Failed] origin. Returns [Some hash] if the
    input is [Cascade _] or [Failed]; [None] otherwise. *)
