(** Valid package list generation.

    Scans build history to produce a list of packages that built successfully,
    for publishing to ocaml.org. *)

val generate : packages_dir:Fpath.t -> string list
(** [generate ~packages_dir] returns package names whose latest build status is
    success. *)

val save : Fpath.t -> string list -> (unit, [> Rresult.R.msg ]) result
(** [save path packages] writes the list as JSON. *)

val load : Fpath.t -> (string list, [> Rresult.R.msg ]) result
(** [load path] reads the list from JSON. *)
