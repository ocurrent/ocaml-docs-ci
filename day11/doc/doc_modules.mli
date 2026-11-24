(** The module paths of every package's documentation, as one file.

    ocaml.org's package search answers "which package provides module X" from
    this export. It is built from the [sidebar.json] that odoc writes for each
    package, and written to the root of an epoch's [html/] directory, so the web
    front end serves it next to the docs:
    [/profiles/<profile>/docs/doc-modules.jsonl.gz].

    The format is ocaml.org's [ocamlorg-doc-modules] version 1, gzipped. The
    first line is a header, [{"format": "ocamlorg-doc-modules", "version": 1}].
    Then comes one object per package, sorted by name in byte order:
    [{"name": "lwt", "version": "5.9.2", "modules": ["Lwt", "Lwt_io", ...]}].
    The output is deterministic: the same docs always give the same bytes, so
    the HTTP ETag only changes when the content does. *)

type package = { name : string; version : string; modules : string list }

val default_max_depth : int
(** By default, module paths deeper than this ([3]) are dropped. *)

val default_max_paths : int
(** By default, at most this many ([500]) paths are kept per package. A few
    packages define more than 100k modules; the AWS SDK, [awso], alone has about
    131k. *)

val filename : string
(** ["doc-modules.jsonl.gz"] *)

val is_module_name : string -> bool
(** An uppercase ASCII letter, then letters, digits, [_] or ['] (the rule
    ocaml.org's decoder applies). *)

val modules_of_sidebar :
  ?max_depth:int -> ?max_paths:int -> Yojson.Safe.t -> string list option
(** The module paths of a [sidebar.json], or [None] if it is not one. A path is
    the chain of module and module type names from the root of the tree, joined
    with dots; library headers and pages add no segment, and source trees are
    skipped. Only paths made of valid module names ({!is_module_name}) and at
    most [max_depth] deep (default {!default_max_depth}) are kept, shallowest
    first and then in byte order, up to [max_paths] (default
    {!default_max_paths}).

    @raise Invalid_argument if [max_depth] or [max_paths] is below 1. *)

val collect : ?max_depth:int -> ?max_paths:int -> Fpath.t -> package list
(** [collect html_dir] reads [html_dir/p/<name>/<version>/doc/sidebar.json] for
    every package. For each one, it keeps the highest version (in opam's order)
    whose sidebar has at least one module path. Packages without such a version,
    and names or versions that ocaml.org would reject, are left out. The result
    is sorted by name. [max_depth] and [max_paths] are as for
    {!modules_of_sidebar}. *)

val to_jsonl : package list -> string
(** The export's uncompressed contents. *)

val write :
  ?max_depth:int ->
  ?max_paths:int ->
  Fpath.t ->
  ([ `Written | `Unchanged ] * int, string) result
(** [write html_dir] collects [html_dir]'s packages and writes them, gzipped, to
    [html_dir/]{!filename}. It returns the number of packages. The file is
    replaced atomically, and left untouched ([`Unchanged]) when its bytes would
    not change. Compression runs [gzip -n], so no timestamp ends up in the
    output. Blocking: reads every package's sidebar. *)
