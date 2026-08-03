(** Persist the planned doc DAG to [<snapshot_dir>/dag.json] so cascade
    attribution can be recovered offline.

    The DAG is computed once per snapshot (a fixed point of opam-repo SHAs +
    profile config), so the on-disk file is also written once per snapshot —
    re-running a snapshot's pipeline overwrites identically since all node
    hashes are content-derived.

    Cascade attribution from the JSON: for any node whose layer-on-disk is
    missing, walk [deps] transitively to find the first ancestor that ran and
    failed. That ancestor is the root cause; everything in between is a cascade.
    No need to record cascades to disk — they're a derived view of (DAG ∩
    on-disk layer state). *)

type kind = Build | Tool | Compile | Doc_all | Link

type entry = {
  hash : string;
  pkg : OpamPackage.t;
  kind : kind;
  deps : string list;
      (** Hashes of direct dependencies, in plan-construction order. *)
  universe : string;
      (** Real output universe — [Day11_doc.Command.compute_universe_hash] of
          the node's build-layer hash, i.e. the [u/<universe>/...] path the docs
          land in (matches the [--parent-id u/...] in the build log). ["" ] for
          tool nodes and for dag.json files predating this field. *)
  blessed : bool;
      (** Whether this node's universe is the blessed one for its package
          (per-universe, not the package-level [is_blessed]). [false] for
          dag.json files predating this field. *)
}

val path : Fpath.t -> Fpath.t
(** [path snapshot_dir] returns the canonical [dag.json] location. *)

val write :
  snapshot_dir:Fpath.t -> entry list -> (unit, [> Rresult.R.msg ]) result
(** [write ~snapshot_dir entries] serialises [entries] to
    [<snapshot_dir>/dag.json]. Overwrites any existing file. *)

val read : snapshot_dir:Fpath.t -> (entry list, [> Rresult.R.msg ]) result
(** [read ~snapshot_dir] reads [<snapshot_dir>/dag.json]. *)
