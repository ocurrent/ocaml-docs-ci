(** Blessing — select canonical documentation per package.

    When the same package appears in multiple solutions (different universes),
    the blessing system picks the "best" universe for each package's canonical
    documentation. *)

val compute_blessings :
  (OpamPackage.t * Day11_solution.Deps.t) list ->
  (OpamPackage.t * bool OpamPackage.Map.t) list
(** [compute_blessings solutions] returns per-target blessing maps. Heuristic:
    maximize deps_count (richer docs), then revdeps_count (stability). A package
    in only one universe is always blessed. *)

val compute_blessed_universes :
  (OpamPackage.t * Day11_solution.Deps.t) list ->
  (OpamPackage.t, Day11_solution.Universe.t) Hashtbl.t
(** [compute_blessed_universes solutions] returns a map from package to the
    universe of its blessed universe. *)

val universe_hash_of_deps : OpamPackage.Set.t -> Day11_solution.Universe.t
(** Compute a universe identifier from a dependency set. *)

val is_blessed : bool OpamPackage.Map.t -> OpamPackage.t -> bool
