val configure :
  base:Spec.t Current.t ->
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  Opamfile.t Current.t

val build :
  base:Spec.t Current.t ->
  project:Current_git.Commit.t Current.t ->
  unikernel:string ->
  target:string ->
  unit Current.t
