type commit = string

val v :
  system:Platform.system ->
  repo:Current_git.Commit.t Current.t ->
  packages:string list Current.t ->
  constraints:(string * string) list Current.t ->
  ((OpamPackage.t * OpamPackage.t list) list * commit) Current.t
(** [v ~system ~repos ~packages] resolves the requested [packages] using the 
  given [repos] on the platform [system]. The arch is hardcoded to x86_64. *)
