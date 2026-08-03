(** Reverse dependency lookup.

    Given a set of solutions, finds which packages transitively depend on a
    given package. *)

val find : Deps.t list -> OpamPackage.t -> OpamPackage.Set.t
(** [find solutions pkg] returns all packages across [solutions] that
    transitively depend on [pkg]. *)
