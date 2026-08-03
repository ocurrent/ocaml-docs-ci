(** Filesystem scanning for layer discovery.

    Enumerates layers and packages from the on-disk cache structure. All
    functions are generic — the caller interprets directory names and applies
    any prefix-based filtering. *)

(** {2 Layer enumeration} *)

val list_layers : Eio_unix.Stdenv.base -> Fpath.t -> (string * Fpath.t) list
(** [list_layers env dir] returns [(name, path)] pairs for all subdirectories
    under [dir]. Returns [[]] if the directory doesn't exist or can't be read.
*)

val list_package_symlinks :
  ?exclude:string list ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  string ->
  (string * string) list
(** [list_package_symlinks ?exclude env packages_dir pkg_str] returns
    [(symlink_name, target)] pairs for all symlinks in [packages_dir/pkg_str/].
    [exclude] is an optional list of symlink names to skip (e.g.
    [["blessed-build"; "blessed-docs"]]). *)
