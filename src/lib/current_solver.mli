val v :
  system:Platform.system ->
  repo:Current_git.Commit.t Current.t ->
  packages:string list Current.t ->
  constraints:(OpamParserTypes.relop * OpamTypes.version) OpamTypes.name_map Current.t ->
  OpamPackage.t list Current.t
(** [v ~system ~repos ~packages] resolves the requested [packages] using the 
  given [repos] on the platform [system]. The arch is hardcoded to x86_64. *)
