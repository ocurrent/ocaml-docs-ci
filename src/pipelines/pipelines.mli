open Mirage_ci_lib

val skeleton :
  repos:(string * Current_git.Commit.t) Current.t list ->
  Current_git.Commit.t Current.t ->
  unit Current.t
(** Mirage-skeleton unikernel builder *)

val monorepo_released :
  repos:(string * Current_git.Commit.t) Current.t list -> Universe.Project.t list -> unit Current.t
(** Monorepo tester -- released *)

val monorepo_edge :
  repos:(string * Current_git.Commit.t) Current.t list -> Universe.Project.t list -> unit Current.t
(** Monorepo tester -- bleeding edge *)
