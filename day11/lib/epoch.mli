(** Epoch management for atomic documentation deployment.

    An epoch is a versioned collection of documentation artifacts.
    The live symlink points to the current epoch; promotion switches
    it atomically. *)

type t = {
  hash : string;
  dir : Fpath.t;
}

val version : string
(** Manual doc-format version, part of {!compute}. Bump it when day11's
    doc-generation logic / HTML layout changes without a doc-tool change. *)

val compute : inputs:string list -> string
(** [compute ~inputs] is the epoch hash for a doc toolchain: a digest of
    {!version} and [inputs] (sorted+deduped). [inputs] is the resolved
    *versions* ([name.version]) of the doc-format-determining tool
    packages — odoc, odoc-driver (voodoo), odoc-md, sherlodoc, odig. We
    key on versions, not the tools' content-addressed build hashes, so a
    deep transitive dep bump in opam-repository no longer churns the
    epoch (and forces a full re-link of the world); only a change to a
    tool that actually shapes the HTML does. Master/overlay builds are
    still captured — they carry the git SHA in the version string.
    Per-package inputs are intentionally excluded — they update
    incrementally within an epoch. Pass the result to {!create}. *)

val create : base_dir:Fpath.t -> string -> t
(** [create ~base_dir hash] creates an epoch directory
    [base_dir/epoch-{hash}/] and returns its handle. *)

val promote : base_dir:Fpath.t -> t -> unit
(** [promote ~base_dir epoch] atomically switches the [html-live]
    symlink to point to [epoch]'s html directory. *)

val current : base_dir:Fpath.t -> t option
(** [current ~base_dir] reads the [html-live] symlink and returns
    the current epoch, or [None] if no epoch is live. *)

val to_gc : base_dir:Fpath.t -> keep:int -> Fpath.t list
(** [to_gc ~base_dir ~keep] is the epoch directories that {!gc} would
    remove — the ones beyond the [keep] most-recent, never including the
    currently-live epoch. Pure and fast (readdir + stat); the caller does
    the actual deletion, which can be very slow and must not run on a
    latency-sensitive event loop. *)

val gc : base_dir:Fpath.t -> keep:int -> int
(** [gc ~base_dir ~keep] removes old epoch directories, keeping
    the [keep] most recent. Returns the number deleted. Note: deletes
    in-process and synchronously — fine for CLI/tests, but callers on an
    event loop should use {!to_gc} and delete out-of-band. *)
