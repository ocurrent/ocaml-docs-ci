type t

val v : Config.t -> Voodoo.t -> t

val digest : t -> string

val pp : t Fmt.t
