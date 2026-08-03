(** Universe tracking for documentation GC.

    Each documentation run produces universe directories ([html/u/{hash}/...]).
    A universe stays alive as long as at least one blessed package references
    it. References are stored in [universes.json] files alongside each package's
    docs. *)

val save_manifest :
  Fpath.t ->
  universe_hash:string ->
  packages:string list ->
  (unit, [> Rresult.R.msg ]) result
(** [save_manifest path ~universe_hash ~packages] writes a universe manifest
    ([universes/{hash}.json]) listing the packages in this universe. *)

val load_manifest : Fpath.t -> (string * string list, [> Rresult.R.msg ]) result
(** [load_manifest path] reads a universe manifest, returning
    [(universe_hash, packages)]. *)

val write_package_refs :
  pkg_html_dir:Fpath.t ->
  universe_hashes:string list ->
  (unit, [> Rresult.R.msg ]) result
(** [write_package_refs ~pkg_html_dir ~universe_hashes] writes [universes.json]
    into the package's HTML directory listing which universes it references.
    Moves atomically with the package docs during {!Atomic_publish}. *)

val collect_referenced : html_dir:Fpath.t -> string list
(** [collect_referenced ~html_dir] scans all [html/p/{pkg}/{ver}/universes.json]
    files and returns the set of universe hashes referenced by any blessed
    package. *)

val gc : html_dir:Fpath.t -> int
(** [gc ~html_dir] deletes universe directories under [html/u/] that are not
    referenced by any blessed package. Returns the number of universes deleted.
*)
