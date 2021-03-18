type t

val v :
  blessed:Package.Blessed.t Current.t -> deps:t list Current.t -> Prep.t Current.t -> t Current.t
