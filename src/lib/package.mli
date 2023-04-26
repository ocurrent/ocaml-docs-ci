module rec Universe : sig
  type t
  (** A dependency universe *)

  val v : Package.t list -> t
  (** [v packages] Build the dependency universe made of [packages] *)

  val deps : t -> Package.t list
  (** Retrieve the list of dependencies *)

  val hash : t -> string
  (** Get the universe hash *)
end

and Package : sig
  type t
  (** A package in the ocaml-docs-ci sense: it's composed of the package name, version, and its dependency universe. *)
end

type t = Package.t

val make :
  blacklist:string list ->
  commit:string ->
  root:OpamPackage.t ->
  (OpamPackage.t * OpamPackage.t list) list ->
  t
(** Using the solver results, obtain the package instance corresponding to the [root] package. *)

val all_deps : t -> t list
(** [all_deps t] is all the dependencies of t, and it includes itself. *)

val pp : t Fmt.t
val compare : t -> t -> int
val opam : t -> OpamPackage.t
val universe : t -> Universe.t
val digest : t -> string
val commit : t -> string
val id : t -> string

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

module Blessing : sig
  type t = Blessed | Universe

  val is_blessed : t -> bool
  val to_string : t -> string

  module Set : sig
    type b = t

    type t
    (** The structure containing which packages are blessed or not. A blessed package is a package aimed to be built for the main documentation pages. *)

    val empty : OpamPackage.t -> t
    (** Construct [t] with an [OpamPackage.t] *)

    val v : counts:int Map.t -> Package.t list -> t
    (** Compute which packages are blessed. *)

    val get : t -> Package.t -> b

    val blessed : t -> Package.t option
    (** Obtain which package is blessed. *)
  end
end
