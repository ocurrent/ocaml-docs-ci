(** Shared writer for per-build bookkeeping: outcome accumulation, packages-dir
    symlinks, and run-log lines.

    Used by [day11 batch] (the one-shot CLI) and by long-running pipelines like
    ocaml-docs-ci that want the same snapshot layout. All entry points are
    thread-safe. *)

type t

val create :
  env:Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  packages_dir:Fpath.t ->
  blessing_maps:(OpamPackage.t * bool OpamPackage.Map.t) list ->
  run_log:Day11_lib.Run_log.t ->
  t

val is_blessed : t -> Day11_opam_layer.Build.t -> bool
(** Whether [node.pkg] is blessed in any of the blessing maps attached at
    [create] time. *)

val record_build : t -> Day11_opam_layer.Build.t -> success:bool -> unit
(** Record a finished build. Appends a {!Day11_lib.History.entry} to
    [packages/<pkg>/history.jsonl] immediately, also buffers a
    {!Summary.build_outcome} for end-of-tick stats, ensures the
    [packages/<pkg>/<hash>] symlink, and writes one ["build"]-kind line to the
    run log. *)

val record_cascade :
  t ->
  failed:Day11_opam_layer.Build.t ->
  failed_dep:Day11_opam_layer.Build.t ->
  unit
(** Record a cascade failure: a build skipped because a dep failed. Creates the
    [packages/<pkg>] symlink and logs the cascade line. Does {b not} write any
    layer state under [<os_dir>/<hash>/], and does {b not} write to
    [history.jsonl] — cascade attribution is fully derivable from
    [<snapshot_dir>/dag.json] plus [<os_dir>/layer_status.jsonl]. *)

val record_doc :
  t ->
  Day11_opam_layer.Build.t ->
  blessed:bool ->
  universe:string ->
  success:bool ->
  unit
(** Record a finished doc-pipeline node (compile / doc-all / link). Appends a
    {!Day11_lib.History.entry} immediately, buffers a {!Summary.doc_outcome},
    ensures the symlink, and writes a ["doc"]-kind line to the run log.
    [blessed] is the {e per-node} (per-universe) blessing from the plan — not
    the package-level {!is_blessed}, which would over-count once a package is
    documented in several universes. *)

val outcomes : t -> Summary.build_outcome list
(** Current accumulated build outcomes (most recent first). Pass to
    {!Summary.finish}/{!Summary.record_history} at end of run. *)

val doc_outcomes : t -> Summary.doc_outcome list
(** Current accumulated doc outcomes (most recent first). *)
