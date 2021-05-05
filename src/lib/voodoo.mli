type t

val v : Config.t -> t Current.t

val cache : Obuilder_spec.Cache.t list

module Prep : sig
  type voodoo = t

  type t

  val spec : base:Spec.t -> t -> Spec.t

  val v : voodoo -> t

  val digest : t -> string
end

module Do : sig
  type voodoo = t

  type t

  val spec : base:Spec.t -> t -> Spec.t

  val v : voodoo -> t

  val digest : t -> string
end
