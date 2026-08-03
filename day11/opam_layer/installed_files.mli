(** Scan a layer's overlay for installed files.

    After a build, the overlay [fs/] directory contains new and changed files.
    These functions scan for documentation-relevant and library files within the
    opam switch prefix. The switch is always ["default"]. *)

val scan_libs : layer_dir:Fpath.t -> string list
(** [scan_libs ~layer_dir] scans [layer_dir/fs/home/opam/.opam/default/lib/] for
    [.cmi], [.cmti], [.cmt], [.cma], [.cmxa], [.cmx], [.ml], [.mli], [META], and
    [dune-package] files. Returns sorted relative paths within [lib/]. *)

val scan_docs : layer_dir:Fpath.t -> string list
(** [scan_docs ~layer_dir] scans [layer_dir/fs/home/opam/.opam/default/doc/] for
    the files [odoc_driver_voodoo] consumes: everything under a package's
    [odoc-pages/] and [odoc-assets/] directories, top-level [.md] files (i.e.
    the [README.md]/[CHANGES.md]/[LICENSE.md] that dune installs as [doc:]
    files), any [.mld], and [odoc-config.sexp]. Returns sorted relative paths
    within [doc/]. *)
