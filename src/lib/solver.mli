module Git = Current_git

type t
type key

val keys : t -> key list
val get : key -> Package.t

val incremental :
  config:Config.t ->
  blacklist:string list ->
  opam:Git.Commit.t Current.t ->
  Track.t list Current.t ->
  t Current.t
