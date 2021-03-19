module Git = Current_git

val track : filter:string list -> Git.Commit.t Current.t -> OpamPackage.t list Current.t
(** Get the list of all packages *)

val solve : opam:Git.Commit.t -> OpamPackage.t Current.t -> Package.t Current.t
(** Get the list of packages obtained when installing this package *)

val select_jobs : targets:Package.t list Current.t -> Package.t list Current.t
(** Obtain the list of jobs to perform to obtain the required packages *)

module Prep = Prep

(*
module Compiled : sig 
  type t
  (* The type for a single package compiled in a folder *)

  val package : t -> Package.Blessed.t
end

val compile : blessed:bool -> deps:Compiled.t list Current.t -> Prep.t Current.t -> Compiled.t Current.t

module Assemble : sig
  type t
  (** The type for an assembled repository *)
end

val assemble_and_link : Prep.t list Current.t -> Compiled.t list Current.t -> Assemble.t Current.t
(** Perform the assemble / link / html steps *)
*)
