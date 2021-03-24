module Git = Current_git

val track : filter:string list -> Git.Commit.t Current.t -> OpamPackage.t list Current.t
(** Get the list of all packages *)

val solve :
  opam:Git.Commit.t -> blacklist:string list -> OpamPackage.t Current.t -> Package.t Current.t
(** Get the list of packages obtained when installing this package *)

val select_jobs : targets:Package.t list Current.t -> Package.t list Current.t
(** Obtain the list of jobs to perform to obtain the required packages *)

module Prep = Prep
