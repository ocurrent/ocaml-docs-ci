type t
(** The opam monorepo tool*)

val v : system:Platform.system -> repos:Repository.fetched list Current.t -> t Current.t
(** Build the opam monorepo tool on [system] using [repos]. *)

val lock :
  value:string ->
  repos:Repository.t list Current.t ->
  opam:Opamfile.t Current.t ->
  t Current.t ->
  Monorepo_lock.t Current.t
(** Perform `opam monorepo lock` on the given [opam] definition using [repos].  *)

val spec : base:Spec.t Current.t -> lock:Monorepo_lock.t Current.t -> unit -> Spec.t Current.t
(** Use opam-monorepo to fetch the lockfile in /src/duniverse *)

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
(** Get the opam file describing a set of projects. *)
