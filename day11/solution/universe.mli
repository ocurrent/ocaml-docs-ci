(** Universe identifier — a hash of the transitive dependency set.

    Used to distinguish different build contexts for the same package version,
    and to determine blessed documentation paths. *)

type t

val of_deps : OpamPackage.Set.t -> t
(** [of_deps deps] computes a universe identifier from a set of transitive
    dependencies. *)

val dummy : t
(** A placeholder for contexts where the universe is not meaningful (e.g. tool
    nodes, test fixtures). *)

val equal : t -> t -> bool
val to_string : t -> string
val pp : Format.formatter -> t -> unit
