type t

val v : repos:(string * Current_git.Commit.t) list Current.t -> t Current.t

val configure :
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  t Current.t ->
  Opamfile.t Current.t

val build :
  base:Spec.t Current.t ->
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  unit Current.t
