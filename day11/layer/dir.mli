(** On-disk layer directory naming convention.

    All layers live in a directory whose name is derived from the layer's
    content hash. This module owns that convention so individual layer kinds
    don't have to know it. *)

val name : string -> string
(** [name hash] returns the directory basename for a layer with full content
    hash [hash]. Uses the first 12 hex characters, producing e.g.
    ["c9f7404f9f87"]. *)

val path : os_dir:Fpath.t -> string -> Fpath.t
(** [path ~os_dir hash] is [os_dir / name hash]. *)
