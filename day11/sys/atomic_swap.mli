(** Atomic directory replacement.

    Implements a staging/commit/rollback pattern for updating a directory
    without partial visibility. On commit, the sequence is:

    + Rename existing final dir to [.old] backup
    + Rename staging dir to final location
    + Remove [.old] backup

    If the process crashes between steps, {!cleanup_stale} recovers on the next
    startup by removing orphaned [.new] and [.old] directories.

    This module has no domain knowledge — the caller provides the target
    directory path directly.

    Several functions require [env] because they may need to remove root-owned
    directories via sudo. *)

val cleanup_stale :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [cleanup_stale ~sw env dir] recursively scans [dir] and removes any
    directories whose names end in [.new] or [.old] (leftovers from crashed
    swaps). Uses sudo for root-owned directories. *)

val prepare : Fpath.t -> (Fpath.t, [> Rresult.R.msg ]) result
(** [prepare target] creates a staging directory at [target ^ ".new"] and
    returns its path. Removes any existing staging dir from a previous failed
    attempt. The caller writes content into the returned path, then calls
    {!commit} or {!rollback}. *)

val commit :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (bool, [> Rresult.R.msg ]) result
(** [commit ~sw env target] atomically swaps the staging directory
    ([target ^ ".new"]) into [target]. Returns [Ok true] if the swap succeeded,
    [Ok false] if there was no staging directory. On failure, attempts to
    restore the previous state. Uses sudo to remove the old directory. *)

val rollback :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [rollback ~sw env target] removes the staging directory ([target ^ ".new"])
    without committing. Uses sudo if needed. *)
