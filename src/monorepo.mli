val monorepo_main :
  analysis:Analyse.Analysis.t Current.t ->
  repos:(string * Current_git.Commit.t) Current.t list ->
  unit ->
  Current_docker.Default.Image.t Current.t
