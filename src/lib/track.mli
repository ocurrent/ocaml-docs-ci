type t [@@deriving yojson]

val digest : t -> string
val pkgs : t -> OpamPackage.t list

val v :
  ?group:bool ->
  limit:int option ->
  filter:string list ->
  Current_git.Commit.t Current.t ->
  t list Current.t
