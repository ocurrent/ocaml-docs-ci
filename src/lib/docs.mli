module Git = Current_git

val track : filter:string list -> Git.Commit.t Current.t -> OpamPackage.t list Current.t
(** Get the list of all packages *)

val solve : opam:Git.Commit.t -> OpamPackage.t Current.t -> Package.t Current.t
(** Get the universe associated to this package *)

val explode : opam:Git.Commit.t -> Universe.t Current.t -> Package.t list Current.t
(** Get all universes contained in this universe *)

val bless_packages : Package.t list Current.t -> Package.Blessed.t list Current.t
(** Find which packages are blessed *)

val get_jobs :
  targets:(Universe.t * Package.t list) list Current.t ->
  blessed:Package.Blessed.t list Current.t ->
  (Universe.t * Package.Blessed.t list) list Current.t
(** The list of jobs to perform, along with the blessed packages *)

module Prep : sig
  type t
  (** The type for prepped universes *)
end

val build_and_prep : opam:Git.Commit.t -> Package.t Current.t -> Prep.t Current.t
(** Install package, run voodoo-prep and push obtained universes. *)

module Assemble : sig
  type t
  (** The type for an assembled repository *)
end

val assemble_and_link : Prep.t list Current.t -> Assemble.t Current.t
(** Perform the assemble / link / html steps *)
