type t

val v :
  system:Matrix.system -> repos:(string * Current_git.Commit_id.t) list Current.t -> t Current.t

val configure :
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  t Current.t ->
  Opamfile.t Current.t

val build :
  ?cmd:string ->
  platform:Matrix.platform ->
  base:Spec.t Current.t ->
  project:Current_git.Commit_id.t Current.t ->
  unikernel:string ->
  target:string ->
  unit ->
  unit Current.t
