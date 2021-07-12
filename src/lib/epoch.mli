type t

val v : Config.t -> Voodoo.t -> t

type stage = [ `Linked | `Html ]

val digest : stage -> t -> string

val pp : t Fmt.t
