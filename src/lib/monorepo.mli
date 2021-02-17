type t

val v : repos:(string * Current_git.Commit.t) list Current.t -> t Current.t

val lock :
  value:string ->
  repos:(string * Current_git.Commit.t) list Current.t ->
  opam:Opamfile.t Current.t ->
  t Current.t ->
  Monorepo_lock.t Current.t

(* Use opam-monorepo to fetch the lockfile in /src/duniverse *)
val spec : base:Spec.t Current.t -> lock:Monorepo_lock.t Current.t -> unit -> Spec.t Current.t

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
