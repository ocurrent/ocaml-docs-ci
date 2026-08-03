(** Solver context.

    Reads packages from a {!Day11_opam.Git_packages.t} index (built from a git
    opam-repository). Implements the interface required by opam-0install's
    solver.

    Supports [prefer_oldest] for reproducible solves, doc/post dependency
    filtering, user constraints, pinned packages, and tracking which packages
    were examined (for incremental reuse). *)

type rejection = UserConstraint of OpamFormula.atom | Unavailable
type t

val create :
  ?prefer_oldest:bool ->
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?doc:bool ->
  ?post:bool ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  env:(string -> OpamVariable.variable_contents option) ->
  packages:Day11_opam.Git_packages.t ->
  unit ->
  t

val candidates :
  t ->
  OpamPackage.Name.t ->
  (OpamPackage.Version.t * (OpamFile.OPAM.t, rejection) result) list

val filter_deps :
  t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamFormula.t

val user_restrictions :
  t -> OpamPackage.Name.t -> OpamFormula.version_constraint option

val env :
  t ->
  OpamPackage.t ->
  OpamVariable.Full.t ->
  OpamVariable.variable_contents option

val pp_rejection : rejection Fmt.t

val examined_packages : t -> OpamPackage.Name.Set.t
(** Returns the set of package names examined during solving. *)

val with_doc_post : doc:bool -> post:bool -> t -> t
(** Create a context with different doc/post settings for recomputing dependency
    edges under alternate filter flags. Internal to the solver library. *)
