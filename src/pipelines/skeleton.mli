open Mirage_ci_lib

val v_main :
  platform:Platform.t ->
  mirage:Current_git.Commit_id.t Current.t ->
  repos:Repository.t list Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t
(** Test mirage-skeleton using the current mirage workflow. *)

val v_4 :
  repos:Repository.t list Current.t ->
  monorepo:Monorepo.t Current.t ->
  platform:Platform.t ->
  Current_git.Commit.t Current.t ->
  unit Current.t
(** Pipeline optimized for mirage 4, using opam-monorepo to track if 
resolutions changes. *)
