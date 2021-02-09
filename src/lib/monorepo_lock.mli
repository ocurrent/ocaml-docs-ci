type t

val make : opam_file:Opamfile.t -> dev_repo_output:string list -> t

val marshal : t -> string

val unmarshal : string -> t

val lockfile : t -> Opamfile.t

type project = { name : string; dev_repo : string; repo : string; packages : string list }

val projects : t -> project list

val commits : t Current.t -> Current_git.Commit.t list Current.t
