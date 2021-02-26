val v :
  remote_push:string ->
  remote_pull:string ->
  branch:string ->
  Current_git.Commit.t list Current.term ->
  Current_git.Commit_id.t Current.term
(** Assemble a submodules monorepo from the given list of commits and push it on [remote_push]. 
Returns a Current_git.Commit.t from which the pushed repo can be retrieved. *)
