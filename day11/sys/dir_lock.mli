(** Directory-level locking.

    Provides mutual exclusion for directory construction. Uses a dual-layer
    locking strategy:

    - {b Intra-process (Eio fibers):} an in-memory mutex table keyed by lock
      path, so fibers within the same process serialize correctly.
    - {b Cross-process:} POSIX [lockf] on a lock file, for the case where two
      separate processes share the same directory.

    This dual-layer approach is necessary because POSIX [lockf] is keyed by
    [(inode, pid)] — two fibers in the same process share a PID and would not
    block each other.

    This module has no domain knowledge — the caller determines where the lock
    file lives and what the marker file is called. *)

val with_lock :
  ?marker_file:Fpath.t ->
  ?lock_file:Fpath.t ->
  Fpath.t ->
  (set_temp_log_path:(Fpath.t -> unit) ->
  Fpath.t ->
  (unit, [ `Msg of string ]) result) ->
  (unit, [ `Msg of string ]) result
(** [with_lock ?marker_file ?lock_file dir_path body] acquires the lock for
    [dir_path], then calls [body ~set_temp_log_path dir_path].

    @param marker_file
      If provided and already exists at [Fpath.(dir_path // marker_file)], the
      body is skipped — another worker already completed this directory.

    @param lock_file
      The path to the lock file. If not provided, defaults to
      [Fpath.(dir_path + ".lock")]. The caller is responsible for choosing a
      meaningful path (e.g. under a central [locks/] directory with a
      descriptive name).

    The [set_temp_log_path] callback updates the lock file metadata with the
    current temp log path, for monitoring by external tools. The lock file also
    records the PID and timestamp.

    The lock is released when [body] returns or raises — even on exceptions, the
    lock file is cleaned up. *)
