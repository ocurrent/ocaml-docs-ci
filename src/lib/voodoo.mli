type t

val v : Config.t -> t Current.t
val cache : Obuilder_spec.Cache.t list
val digest : t -> string

module Prep : sig
  type voodoo = t
  type t

  val spec : base:Spec.t -> t -> Spec.t
  val v : voodoo -> t
  val digest : t -> string
  val commit : t -> Current_git.Commit_id.t
end

module Do : sig
  type voodoo = t
  type t

  val spec : base:Spec.t -> t -> Spec.t
  val v : voodoo -> t
  val digest : t -> string
  val commit : t -> Current_git.Commit_id.t
end

module Gen : sig
  type voodoo = t
  type t

  val spec : base:Spec.t -> t -> Spec.t
  val v : voodoo -> t
  val digest : t -> string
  val commit : t -> Current_git.Commit_id.t
end
