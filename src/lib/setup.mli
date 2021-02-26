val add_repositories : Repository.t list -> Obuilder_spec.op list
(** The obuilder rules to add opam repositories. *)

val install_tools : string list -> Obuilder_spec.op list
(** The obuilder rules to opam install the given list of tools. *)

val tools_image :
  system:Platform.system ->
  ?name:string ->
  Current_solver.resolution list Current.t ->
  Current_docker.Default.Image.t Current.t
(** [tools_image ~system ~name resolutions] generates a docker image containing 
the resolved packages. [name] is used to label the image. *)

val opam_download_cache : Obuilder_spec.Cache.t
(** Obuilder cache for opam downloads *)

val remote_uri : Current_git.Commit_id.t -> string
(** Get the opam-compatible URI of the commit. *)

val network : string list
