module Git = Current_git

val incremental :
  blacklist:string list ->
  opam:Git.Commit.t Current.t ->
  Track.t list Current.t ->
  Package.t list Current.t
