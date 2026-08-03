(** Per-snapshot universe membership metadata.

    A {e universe} is a hash of a doc-dependency closure (see
    {!Day11_solution.Universe}); the hash alone can't be reversed to the package
    set it stands for. When all solutions for a snapshot are known we persist
    that mapping so the dashboard can show, for any universe, the exact package
    versions it contains.

    Layout, relative to a snapshot directory:
    - [universes/index.json] — [{ "universes": [hash, ...] }]
    - [universes/<hash>.json] —
      [{ "universe_hash": hash, "packages": [name.version, ...] }] *)

type t = {
  hash : string;
  packages : string list;  (** [name.version] strings, sorted. *)
}

val dir : Fpath.t -> Fpath.t
(** [dir snapshot_dir] is the [universes/] subdirectory. *)

val write_all :
  snapshot_dir:Fpath.t ->
  (string * string list) list ->
  (unit, [> Rresult.R.msg ]) result
(** [write_all ~snapshot_dir universes] writes the index and one manifest file
    per universe. Each element is [(universe_hash, packages)]. Overwrites any
    existing files. *)

val read_index : snapshot_dir:Fpath.t -> string list
(** [read_index ~snapshot_dir] returns all universe hashes recorded for the
    snapshot, or [[]] if none have been written. *)

val read_manifest : snapshot_dir:Fpath.t -> hash:string -> t option
(** [read_manifest ~snapshot_dir ~hash] returns the package set of a single
    universe, or [None] if not recorded. *)
