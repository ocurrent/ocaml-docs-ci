type t
(** Represents the output of opam monorepo lock, with additional metadata on the dev repositories. *)

val make : opam_file:Opamfile.t -> dev_repo_output:string list -> t
(** [make ~opam_file ~dev_repo_output] parses the lockfile and the dev repo output. *)

val marshal : t -> string

val unmarshal : string -> t

val lockfile : t -> Opamfile.t
(** Get the lockfile back *)

type project = { name : string; dev_repo : string; repo : string; packages : string list }

val projects : t -> project list
(** Get the list of projects (=repositories) of this lockfile *)

val commits : ?filter:(project -> bool) -> t Current.t -> Current_git.Commit.t list Current.t
(** Resolve the dev repositories to find the commits of each main branch. [filter] can be used to 
select specific repositories.*)
