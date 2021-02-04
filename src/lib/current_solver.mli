type resolution = { name : string; version : string; opamfile : Opamfile.t }

val v :
  repos:(string * Current_git.Commit.t) list Current.t ->
  packages:string list ->
  resolution list Current.t
