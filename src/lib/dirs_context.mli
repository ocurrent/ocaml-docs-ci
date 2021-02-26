include Opam_0install.S.CONTEXT

val create :
  ?prefer_oldest:bool ->
  ?test:OpamPackage.Name.Set.t ->
  ?pins:(OpamTypes.version * OpamFile.OPAM.t) OpamTypes.name_map ->
  constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  env:(string -> OpamVariable.variable_contents option) ->
  string list ->
  t
(** create a directory context for the 0install solver. *)

val get_opamfile : t -> OpamPackage.t -> OpamFile.OPAM.t
