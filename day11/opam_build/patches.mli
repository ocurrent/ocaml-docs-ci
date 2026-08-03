(** Package patches: load, hash, and apply via opam-build --patch.

    Patches are stored in a directory tree keyed by package name and version.
    When present, they are bind-mounted into the build container and passed to
    opam-build as [--patch] arguments. *)

type t

val create : Fpath.t -> t
(** [create dir] creates a patch set rooted at [dir]. Patches for package
    [name.version] are expected at [dir/name/version/*.patch]. *)

val has_patches : t -> OpamPackage.t -> bool
(** [has_patches t pkg] returns true if any patches exist for [pkg]. *)

val patches_for : t -> OpamPackage.t -> string list
(** [patches_for t pkg] returns the list of patch file paths for [pkg]. *)

val hash_for : t -> OpamPackage.t -> string
(** [hash_for t pkg] returns a content hash of all patches for [pkg], used to
    include patches in layer hash computation. *)

val patch_args : t -> OpamPackage.t -> string
(** [patch_args t pkg] returns the [--patch] arguments for opam-build's command
    line as a single string. *)

val patch_filenames : t -> OpamPackage.t -> string list
(** [patch_filenames t pkg] returns just the patch filenames (without directory
    path). *)
