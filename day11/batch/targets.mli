(** Target package resolution for batch builds.

    Determines which packages to solve and build, from CLI arguments:
    - No target + [--small-universe]: a curated list of ~20 packages
    - No target: all latest versions from the package index
    - [@filename]: load from a JSON file
    - [pkg.version]: a single explicit target *)

val small_universe : string list
(** Package names for the small universe. *)

val pick_latest_version :
  Day11_opam.Git_packages.t -> string -> OpamPackage.t list
(** [pick_latest_version packages name] returns all non-avoid versions of [name]
    from newest to oldest. Used for retry on solve failure. *)

val find_all_versions : Day11_opam.Git_packages.t -> OpamPackage.t list
(** All versions of all non-compiler packages. *)

val resolve :
  ?small:bool ->
  ?all_versions:bool ->
  Day11_opam.Git_packages.t ->
  string option ->
  OpamPackage.t list
(** [resolve ?small ?all_versions packages target] resolves the target
    specification to a list of packages. When [all_versions] is true and no
    target is given, returns all versions instead of just the latest. *)
