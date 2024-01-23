type t

val v : Voodoo.t -> t

type stage = [ `Linked | `Html ]

val digest : stage -> t -> string
val pp : t Fmt.t
