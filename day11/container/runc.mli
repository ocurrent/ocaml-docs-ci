(** OCI container runtime: running and deleting containers via [runc].

    Thin wrapper over the [runc] command-line runtime. Both functions require
    [sudo] (runc uses Linux clone/namespace syscalls that need [CAP_SYS_ADMIN]).

    A container is identified by the pair of a {e bundle directory} on disk and
    a {e container id} string. The bundle directory must contain a [config.json]
    (OCI spec) and typically a rootfs path referenced from that spec. Use
    {!Oci_spec.make} + {!Oci_spec.write} to produce the config. Container ids
    are caller-chosen but must be unique on the host at the time of the [run]
    call — {!delete} cleans up any stale container with the same id first. *)

val run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  bundle:Fpath.t ->
  container_id:string ->
  (Day11_sys.Run.t, [> Rresult.R.msg ]) result
(** [run ~sw env ~bundle ~container_id] executes
    [sudo runc run -b bundle container_id] and blocks until the container exits.

    {b Side effect:} runc's stdout and stderr are captured to [bundle/runc.log].
    Callers that need to inspect the output can either read that file after the
    call returns, or use the {!Day11_sys.Run.t} value, whose [output]/[errors]
    fields contain the same data.

    The returned {!Day11_sys.Run.t} carries the container's final exit status in
    its [status] field: [`Exited n] for a normal exit and [`Signaled n] if the
    container was killed by a signal. A failed {e invocation} of runc itself
    (e.g. runc missing from [$PATH]) returns [Error _]; a non-zero exit from a
    container that ran to completion returns [Ok run] with a non-zero
    [run.status]. Callers must distinguish the two.

    This function does not clean up the container after it exits. Always pair it
    with {!delete} inside a [Fun.protect] so that exceptions don't leak
    container state into runc's on-disk registry at [/run/runc/]. *)

val delete :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  string ->
  (unit, [> Rresult.R.msg ]) result
(** [delete ~sw env container_id] runs [sudo runc delete -f container_id] to
    forcefully remove a container from runc's registry, terminating it if it's
    still running. Safe to call on a non-existent id — the error is returned and
    can be ignored.

    Callers typically [ignore] the result, both before a run (to clear any stale
    state from a previous run that didn't clean up) and after, inside a
    [Fun.protect] finally handler. *)
