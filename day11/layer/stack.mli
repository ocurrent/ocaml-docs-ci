(** Layer stacking — merge multiple layer filesystems into one directory.

    Layers are stacked in order: earlier layers are lower (overridden by later
    ones). The merge uses [sudo cp --archive --link] for speed — hardlinks avoid
    copying file data. Sudo is required because layer [fs/] directories are
    typically root-owned (created by container builds). *)

val merge :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  layer_dirs:Fpath.t list ->
  target:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [merge ~sw env ~layer_dirs ~target] stacks the [fs/] subdirectory of each
    layer in [layer_dirs] into [target], in order. Uses [sudo cp -an --link] so
    files are hardlinked, not copied. Later layers override earlier ones ([-n]
    means no-clobber, so first writer wins — pass layers in reverse dep order if
    needed).

    Returns [Error] if any copy fails (e.g. a layer dir doesn't exist). *)

val merge_no_sudo :
  Eio_unix.Stdenv.base ->
  layer_dirs:Fpath.t list ->
  target:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** Like {!merge}, but runs [cp] directly without sudo. Intended for user-owned
    layers (e.g. those produced by the native backend). *)

val plan_lowerdir :
  available:int ->
  merged_overhead:int ->
  entry_cost:(Fpath.t -> int) ->
  Fpath.t list ->
  Fpath.t list * Fpath.t list
(** [plan_lowerdir ~available ~merged_overhead ~entry_cost layer_dirs] decides
    which layers to pass as separate overlayfs lowerdirs and which to cp-merge
    into a single shared dir, so that the resulting mount options string fits in
    the caller's byte budget.

    Returns [(separate, to_merge)]. The fast path (when all layers fit) returns
    [(layer_dirs, [])] with no merging needed. When some must be merged, as many
    as possible are kept separate (taken from the front of the list).

    Parameters:
    - [available]: the byte budget for the dep-entry portion of the mount
      options string. The caller is responsible for subtracting fixed overhead
      (keyword, separators, base/fs entry, upperdir, workdir) before passing
      this value.
    - [merged_overhead]: extra bytes added to the cost when at least one layer
      must be merged (typically the length of the merged-lower path plus its
      leading colon separator).
    - [entry_cost]: function returning how many bytes a single separate-lowerdir
      entry contributes (typically
      [String.length (Fpath.to_string (dir / "fs")) + 1] for the colon). *)
