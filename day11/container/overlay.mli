(** Linux overlayfs mount and umount.

    Thin wrapper over the [mount(8)] and [umount(8)] commands that requires
    [sudo]. The library supplies exactly the primitives needed to stack build
    layers into a single rootfs for a container run:

    - {!mount} assembles an overlayfs from any number of read-only lowers, one
      writable upper, and one workdir.
    - {!umount} tears it down.

    The mount is the final filesystem the container sees as [/]. The upper dir
    captures everything the container writes; after the run, the caller
    typically renames the upper into its cache as a new layer. *)

val mount :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  lower:Fpath.t list ->
  upper:Fpath.t ->
  work:Fpath.t ->
  target:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [mount ~sw env ~lower ~upper ~work ~target] mounts an overlayfs at [target]
    using the given lowers and upper.

    {b Lower ordering follows kernel convention: the {e first} entry in [lower]
       becomes the {e topmost} layer}, shadowing files from later entries with
    the same path. The list is joined as [lowerdir=A:B:C:...] and passed to
    [mount -t overlay], so the same precedence rules as the [lowerdir] option
    apply:

    - Directories from all lowers are merged, union-style.
    - For non-directory entries with the same name, the leftmost layer wins.
    - A [trusted.overlay.opaque=y] xattr on a directory in the upper causes the
      corresponding directory in the lower to be hidden (does not apply to
      directories in lowers themselves).
    - A whiteout (char device 0/0) in a higher layer hides a same-named entry in
      a lower layer.

    The [upper] and [work] directories must exist and must be on the same
    filesystem. The upper directory receives any writes the container makes to
    its rootfs. The work directory is internal overlayfs scratch space and
    should be treated as opaque — do not read or write it directly.

    Mount-option length limit: the classic [mount(2)] syscall caps the options
    string at [PAGE_SIZE] (typically 4096 bytes). With many lowers or long
    paths, this limit can be hit; {!Day11_layer.Stack.plan_lowerdir} is the
    recommended way to split a large dep list into a "kept separate" bucket and
    a "cp-merged into one lower" bucket that fits the budget.

    @param lower Read-only lowers, leftmost = topmost. Must be non-empty.
    @param upper
      Writable upper directory (must be on the same filesystem as [work]).
    @param work Overlayfs workdir — scratch, do not touch.
    @param target Mount point. Must already exist as an empty directory.
    @return
      [Ok ()] on success, or [Error (`Msg _)] if [mount] fails. Common failure
      modes include a too-long options string, a missing directory, or a
      permission error. *)

val umount :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [umount ~sw env target] unmounts the overlay previously mounted at [target].
    Safe to call on a directory that is not currently a mount point — the error
    is returned but callers typically [ignore] it inside cleanup handlers. *)
