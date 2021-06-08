type t
(** The type for a prepped package (build objects in a universe/package folder) *)

val commit_hash : t -> string

val tree_hash : t -> string

val package : t -> Package.t

type prep_result = [`Cached | `Success of t | `Failed of t]

type prep

val extract : job:Jobs.t -> prep Current.t -> prep_result Current.t Package.Map.t

val v : config:Config.t -> voodoo:Voodoo.Prep.t Current.t -> Jobs.t -> prep Current.t
(** Install a package universe, extract useful files and push obtained universes on git. *)

val folder : Package.t -> Fpath.t

val pp : t Fmt.t

val compare : t -> t -> int
