type t [@@deriving yojson]

val digest : t -> string
val pkg : t -> OpamPackage.t
val v : limit:int option -> filter:string list -> Current_git.Commit.t Current.t -> t list Current.t

module Map : OpamStd.MAP with type key = t
