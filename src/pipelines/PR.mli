module Github = Current_github
module Git = Current_git

type t
(** The PR tester *)

val make : Github.Api.t -> (string * Git.Commit_id.t) list Current.t -> t

val to_current : t -> unit Current.t

val routes : t -> Current_web.Resource.t Routes.route list
