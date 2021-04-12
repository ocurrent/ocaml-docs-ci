type commit = string

val v :
  system:Platform.system ->
  repo:Current_git.Commit.t Current.t ->
  packages:string list Current.t ->
  constraints:(string * string) list Current.t ->
  ((OpamPackage.t * OpamPackage.t list) list * commit) Current.t
(** [v ~system ~repos ~packages ~constraints] resolves the requested [packages] using the 
  given [repo] on the platform [system]. The arch is hardcoded to x86_64. It accepts a set of [constraints],
  (name, version) pairs that lock given packages to specific versions. 
  
  The output is the list of packages that needs to be installed, as well their own dependency universes. *)
