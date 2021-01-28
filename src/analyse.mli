module Analysis : sig
  type t

  val lockfile : t -> OpamParserTypes.opamfile

  type project = { name : string; dev_repo : string; repo : string; packages : string list }

  val projects : t -> project list
end

val v :
  repos:(string * Current_git.Commit.t) Current.t list ->
  packages:Universe.Project.t list ->
  ?with_test:bool ->
  unit ->
  Analysis.t Current.t
