
val monorepo_master :
  repos:(string * Current_git.Commit.t) Current.t list ->
  projects:(string * Current_git.Commit.t) Current.t list -> unit -> Current_docker.Default.Image.t Current.t
