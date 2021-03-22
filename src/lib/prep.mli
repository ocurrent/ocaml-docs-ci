type t
(** The type for a prepped package (build objects in a universe/package folder) *)

val package : t -> Package.t

val v : Package.t Current.t -> t list Current.t
(** Install a package universe, extract useful files and push obtained universes. *)

val folder : t -> Fpath.t
