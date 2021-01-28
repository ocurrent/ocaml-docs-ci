val monorepo_main :
  base:Spec.t Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit ->
  Current_docker.Default.Image.t Current.t

val monorepo_released :
  base:Spec.t Current.t ->
  lock:Monorepo_lock.t Current.t ->
  unit ->
  Current_docker.Default.Image.t Current.t

val lock :
  base:Spec.t Current.t ->
  (* base opam setup *)
  opam:Opamfile.t Current.t ->
  Monorepo_lock.t Current.t

val opam_file : ocaml_version:string -> Universe.Project.t list -> Opamfile.t
