(** Keeping rendered HTML in doc layers and publishing it into epochs.

    A doc-all or link node renders its package's HTML into a scratch dir
    ({!capture} then moves it into [<layer>/html/]). The layer hash covers
    everything that shapes that HTML (universe, blessing, tools, dep doc
    layers), so the same hash always means the same pages, whichever profile
    built it.

    An epoch's [html/] tree is then assembled from layers: {!publish} hardlinks
    each package-version dir of a layer into the epoch, and {!reconcile} does
    that for every layer a profile plans. This is what lets two profiles that
    share a doc layer both get its pages: the second one never re-runs the build
    (OCurrent dedupes on the layer hash), but reconcile still links the pages
    into its epoch. Hardlinks keep published pages alive when the layer GC later
    deletes the layer.

    A publish unit is a package-version dir: [p/<name>/<version>] for a blessed
    universe, [u/<universe>/<name>/<version>] otherwise (see
    {!Odoc_store.rel_path}). Each published unit carries a [.day11-layer] marker
    holding the source layer's hash, so re-publishing an unchanged unit is one
    small file read.

    Everything else odoc writes at the root of the HTML tree ([odoc.css],
    [fonts/], [katex.min.js], ...) is the same for every package rendered by a
    given toolchain. Keeping it in every layer would cost ~1 MB per layer, so
    {!capture} moves it once into a shared support dir keyed by the toolchain,
    and {!publish} links it into the epoch root. *)

val layer_html : Fpath.t -> Fpath.t
(** [layer_html layer_dir] is [layer_dir/html], where a doc layer keeps its
    rendered package-version dirs. *)

val marker_name : string
(** Name of the per-unit marker file ([.day11-layer]). *)

val capture :
  src:Fpath.t ->
  layer_dir:Fpath.t ->
  support_root:Fpath.t ->
  support_key:string ->
  (unit, [> Rresult.R.msg ]) result
(** [capture ~src ~layer_dir ~support_root ~support_key] moves a freshly
    rendered HTML tree [src] into [layer_html layer_dir]. The [p/] and [u/]
    subtrees go into the layer; every other root entry goes to
    [support_root/support_key/] if that dir doesn't exist yet, and is dropped
    otherwise. The layer records [support_key] so {!publish} can find the
    support files again. [src] is consumed. *)

type outcome =
  | No_html
      (** The layer has no [html/] dir: it predates HTML-in-layer, or it is a
          compile-only layer. *)
  | Published of { units : int; skipped : int }
      (** [units] package-version dirs were (re)linked; [skipped] were already
          up to date. *)

val publish :
  epoch_html:Fpath.t ->
  support_root:Fpath.t ->
  hash:string ->
  Fpath.t ->
  (outcome, [> Rresult.R.msg ]) result
(** [publish ~epoch_html ~support_root ~hash layer_dir] links every unit of
    [layer_dir]'s HTML into [epoch_html], replacing a unit whose marker names a
    different layer. A replaced unit is swapped in whole (built beside the
    target, then renamed into place), so stale files from an older build don't
    linger. Support files are linked into the epoch root the first time any
    layer is published there. *)

type stats = {
  layers : int;
  no_html : int;
  units : int;
  skipped : int;
  errors : (string * string) list;  (** (layer hash, message) *)
}

val reconcile :
  epoch_html:Fpath.t -> support_root:Fpath.t -> (string * Fpath.t) list -> stats
(** [reconcile ~epoch_html ~support_root layers] {!publish}es each
    [(hash, layer_dir)]. A failure on one layer is recorded in [errors] and
    doesn't stop the rest. Blocking filesystem work: run it off the event loop.
*)
