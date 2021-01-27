val v :
  repo_mirage_skeleton:Current_git.Local.t ->
  repo_mirage_dev:Current_git.Local.t ->
  repo_mirage_ci:Current_git.Local.t ->
  unit ->
  unit Current.t

val v2 :
  repo_opam:Current_git.Commit.t Current.t ->
  repo_overlays:Current_git.Commit.t Current.t ->
  repo_mirage_dev:Current_git.Local.t ->
  Universe.Project.t list ->
  unit Current.t
