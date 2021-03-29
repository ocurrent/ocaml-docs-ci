type t
(** The type for a prepped package (build objects in a universe/package folder) *)

val package : t -> Package.t

val v : voodoo:Voodoo.t Current.t -> digests:Folder_digest.t Current.t -> Jobs.t Current.t -> t list Current.t
(** Install a package universe, extract useful files and push obtained universes. *)

val folder : t -> Fpath.t

val artifacts_digest : t -> string
