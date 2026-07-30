(** Per-layer "last used" timestamp for LRU cache eviction.

    Each layer has a [last_used] sentinel file in its directory whose
    mtime records the most recent access. The file's content is
    irrelevant — only its mtime matters.

    This is deliberately split off from {!Meta} so that marking
    a layer as used is cheap — just [utimensat] on a small sentinel
    file, no JSON read/write. *)

val touch : Eio_unix.Stdenv.base -> Fpath.t -> unit
(** [touch env layer_dir] records that the layer has just been accessed.
    Creates [layer_dir/last_used] if it doesn't exist, or updates its
    mtime if it does. Errors are silently ignored — touch must never
    fail a build. *)

val get : Eio_unix.Stdenv.base -> Fpath.t -> float option
(** [get env layer_dir] returns the unix timestamp (seconds since epoch)
    of the last touch, or [None] if the sentinel file doesn't exist
    or can't be stat'd. *)

val effective : Eio_unix.Stdenv.base -> Fpath.t -> float option
(** [effective env layer_dir] is the best available "last used" time for
    eviction decisions: the sentinel mtime when there is one, else the
    layer's own [layer.json] mtime.

    The fallback matters because {!touch} is only called when a layer is
    {e re-used} (a cache hit, or stacked as an overlay lower). A layer
    that was built and then only ever bind-mounted — the odoc /
    odoc-driver tool layers are the case that bit us — has no sentinel
    however fresh it is, and treating that as "epoch 0" makes an LRU
    sweep delete it immediately. Deleting a tool layer that the
    OCurrent cache still records as built strands every doc node behind
    it (see [Day11_prep.reconcile_cache] in ocaml-docs-ci).

    [None] only when the layer has neither sentinel nor [layer.json] —
    residue from a failed attempt, which stays freely evictable. *)
