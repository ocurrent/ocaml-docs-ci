(** Filesystem snapshots — lightweight before/after comparison.

    Designed for the native build backend: snapshot a prefix before a build, run
    the build, diff the prefix to get the list of files produced.

    The snapshot records [(size, mtime, inode)] per file. This catches:
    - new files (absent from [before])
    - modified files (different mtime or size)
    - in-place replacements (same mtime, different inode) — e.g. a build script
      that does [mv newfile oldfile] with [cp -p]

    Content hashing is deliberately avoided — it would dominate build time. For
    opam-style builds, where files are typically written once and never modified
    in place, [(size, mtime, inode)] is sufficient. *)

type entry = { size : int64; mtime : float; inode : int64 }
type t

val take : Eio_unix.Stdenv.base -> Fpath.t -> t
(** [take env root] walks [root] (recursively) and records one {!entry} per
    regular file and symlink, keyed by path relative to [root]. Directories are
    not recorded, but are traversed. Missing [root] returns an empty snapshot.
*)

val diff : Eio_unix.Stdenv.base -> before:t -> Fpath.t -> string list
(** [diff env ~before root] walks [root] again and returns the list of relative
    paths that are either:
    - absent from [before] (new files), or
    - present in [before] but with a different [(size, mtime, inode)]

    Paths are relative to [root]. Symlinks are compared by their target's
    size/mtime/inode — not by the symlink's own metadata — since
    [Eio.Path.stat ~follow:true] follows symlinks. *)

val is_empty : t -> bool
val size : t -> int
