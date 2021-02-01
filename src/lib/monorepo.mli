val lock : base:Spec.t Current.t -> opam:Opamfile.t Current.t -> Monorepo_lock.t Current.t

val monorepo_main :
  base:Spec.t Current.t -> lock:Monorepo_lock.t Current.t -> unit -> Spec.t Current.t

val monorepo_released :
  base:Spec.t Current.t -> lock:Monorepo_lock.t Current.t -> unit -> Spec.t Current.t

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
