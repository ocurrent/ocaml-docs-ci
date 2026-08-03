type t

val cmdliner : t Cmdliner.Term.t
val jobs : t -> int
val track_packages : t -> string list
val take_n_last_versions : t -> int option
