type resolution = { name : string; version : string; opamfile : Opamfile.t }

val v :
  system:Platform.system ->
  repos:Repository.t list Current.t ->
  packages:string list ->
  resolution list Current.t
(** [v ~system ~repos ~packages] resolves the requested [packages] using the 
  given [repos] on the platform [system]. The arch is hardcoded to x86_64. *)
