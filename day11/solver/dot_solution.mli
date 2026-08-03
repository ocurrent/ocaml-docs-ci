(** Graphviz DOT output for dependency solutions. *)

val to_string : OpamPackage.Set.t OpamPackage.Map.t -> string
(** [to_string solution] renders the dependency graph as a DOT digraph. *)

val save :
  Fpath.t ->
  OpamPackage.Set.t OpamPackage.Map.t ->
  (unit, [> Rresult.R.msg ]) result
(** [save path solution] writes the DOT graph to [path]. *)
