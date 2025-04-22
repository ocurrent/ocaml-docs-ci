type t
(** The type for a prepped package (build objects in a universe/package folder)
*)

module OpamFiles : sig
  type t = No_context

  module Key : sig
    type t = { repo : Current_git.Commit.t; packages : OpamPackage.t list }
  end

  module Value : sig
    type t = (bool * string, [ `Msg of string ]) result OpamPackage.Map.t
    [@@deriving yojson]
  end
end

module OpamFilesCache : sig
  val get :
    ?schedule:Current_cache.Schedule.t ->
    OpamFiles.t ->
    OpamFiles.Key.t ->
    OpamFiles.Value.t Current.Primitive.t
end

val hash : t -> string
val prep_hash : t -> string
val package : t -> Package.t
val base : t -> Spec.t

type prep_result = Success | Failed

val result : t -> prep_result

type prep

val add_base : Ocaml_version.t -> OpamPackage.t list -> OpamPackage.t list

val v :
  config:Config.t ->
  spec:Spec.t Current.t ->
  deps:t list Current.t ->
  opamfiles:OpamFiles.Value.t Current.t ->
  prep:Package.t ->
  t Current.t
(** Install a package universe, extract useful files and push obtained universes
    on git. *)

val pp : t Fmt.t
val compare : t -> t -> int
