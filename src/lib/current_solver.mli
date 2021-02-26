type resolution = { name : string; version : string; opamfile : Opamfile.t }

val v :
  system:Matrix.system ->
  repos:(string * Current_git.Commit_id.t) list Current.t ->
  packages:string list ->
  resolution list Current.t
