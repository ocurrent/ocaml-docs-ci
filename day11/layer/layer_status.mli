(** Durable per-os-dir log of layer finalisation outcomes.

    Stored at [<os_dir>/layer_status.jsonl] as one JSON object per line:
    [{"hash":"…32hex","exit_status":0,"ts":"…"}]. Append-only; last entry per
    hash wins on read.

    This file exists so callers can answer "what's the durable state of every
    layer?" without doing 25k+ small file reads against [layer.json] sidecars.
    {!load} reads and parses one file; {!append} writes one line per
    layer-finalisation event.

    Bootstrap: on first {!load} the file is created by walking
    [<os_dir>/*/layer.json] and reading each meta. That's slow (~7s on a
    25k-layer cache), but it only happens once. Every subsequent finalisation
    appends, so {!load} stays cheap. *)

type entry = { hash : string; exit_status : int; ts : string }

val path : Fpath.t -> Fpath.t

val append : os_dir:Fpath.t -> hash:string -> exit_status:int -> unit
(** Atomic single-syscall append, serialised within the process by an internal
    mutex so multiple build workers finishing simultaneously don't tear lines.
    Uses [O_APPEND] for protection against external writers (other day11
    processes against the same os_dir). *)

val load : os_dir:Fpath.t -> (string, entry) Hashtbl.t
(** [load ~os_dir] returns [hash → latest entry] for every layer ever recorded.
    Bootstraps the file by walking on-disk layers if it doesn't exist yet — that
    first call takes seconds, every later call milliseconds. *)
