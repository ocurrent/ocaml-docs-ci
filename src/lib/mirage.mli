type t
(** The type representing the mirage tool. *)

val v : system:Platform.system -> repos:Repository.fetched list Current.t -> t Current.t
(** [v ~system ~repos] Build the mirage tool on [system] using [repos]. It's always built for the host 
machine. *)

val configure :
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  t Current.t ->
  Opamfile.t Current.t
(** Run `mirage configure -t [target]` in the [unikernel] folder of the [project] repository and return 
the generated opam file. It runs on the host machine. *)

val build :
  ?cmd:string ->
  platform:Platform.t ->
  base:Spec.t Current.t ->
  project:Current_git.Commit_id.t Current.t ->
  unikernel:string ->
  target:string ->
  unit ->
  unit Current.t
(** Run the full mirage build process using ocluster. It includes the installation of mirage, the 
configuration step and the build step. The build command can be customized with the [cmd] parameter. 
By default it's `dune build` (mirage 4). *)
