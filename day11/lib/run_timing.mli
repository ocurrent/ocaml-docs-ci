(** Wall-clock timing for a snapshot's first build-to-completion.

    Two write-once markers in the snapshot dir: [started_at] (first time the
    snapshot enters the pipeline) and [completed_at] (first completion).
    Persisted so the duration is stable across the daemon's frequent
    pipeline re-evaluations and restarts. *)

val record_start : dir:Fpath.t -> unit
(** [record_start ~dir] stamps the snapshot's start time, once. A no-op if a
    start was already recorded — so a re-evaluation, or a snapshot carried
    over from a previous daemon session, keeps its original start. *)

val duration : dir:Fpath.t -> float option
(** [duration ~dir] returns the build duration in seconds
    ([completed_at - started_at]), stamping [completed_at] on first call.
    Stable on repeat calls (the first completion time is kept). [None] only
    if no start was ever recorded. *)
