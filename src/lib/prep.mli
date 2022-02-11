type t
(** The type for a prepped package (build objects in a universe/package folder) *)

val hash : t -> string
val package : t -> Package.t
val base : t -> Spec.t

type prep_result = Success | Failed

val result : t -> prep_result

type prep

val extract : job:Jobs.t -> prep Current.t -> t Current.t Package.Map.t

val v : config:Config.t -> voodoo:Voodoo.Prep.t Current.t -> spec:Spec.t -> Jobs.t -> prep Current.t
(** Install a package universe, extract useful files and push obtained universes on git. *)

val pp : t Fmt.t
val compare : t -> t -> int
