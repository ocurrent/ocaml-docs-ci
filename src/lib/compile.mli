type t

val is_blessed : t -> bool

val package : t -> Package.t

val folder : t -> string

val v :
  blessed:Package.Blessed.t Current.t -> deps:t list Current.t -> Prep.t Current.t -> t Current.t
