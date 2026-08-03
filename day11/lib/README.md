# lib — Run management & reporting

Run lifecycle, progress tracking, per-package build history, status
reporting, garbage collection, and notifications.

This library has minimal dependencies and is also used by the web
dashboard for reading run/progress data.

## External dependencies

- `unix`
- `yojson`

Does NOT depend on `exec`, `layer`, `container`, or any opam libraries.
This is intentional — it's a pure data management library.

## Modules (carried forward from day10_lib)

### `Run_log` — run lifecycle

```ocaml
type t  (** opaque run metadata *)
type summary  (** run_id, start/end times, counts, failures *)

val set_log_base_dir : string -> unit
val start_run : unit -> t
val get_id : t -> string
val get_run_dir : t -> string
val get_start_time : t -> float
val format_time : float -> string
val add_build_log : t -> package:string -> source_log:string -> unit
val add_doc_log : t -> package:string -> source_log:string -> layer_hash:string -> ?... -> unit
val finish_run : t -> targets_requested:int -> ... -> summary
```

### `Progress` — batch progress tracking

```ocaml
type phase = Solving | Blessings | Building | Gc | Completed
type t

val create : run_id:string -> start_time:string -> targets:string list -> t
val set_phase : t -> phase -> t
val set_solutions : t -> found:int -> failed:int -> t
val set_build_total : t -> int -> t
val set_completed : t -> build:int -> doc:int -> t
val to_json : t -> Yojson.Safe.t
val write : run_dir:string -> t -> unit
val delete : run_dir:string -> unit
```

### `History` — per-package build history

Append-only JSONL files at `packages/{pkg}/history.jsonl`.

```ocaml
type entry = {
  ts : string; run : string; build_hash : string;
  status : string; category : string; compiler : string;
  blessed : bool; error : string option;
  failed_dep : string option; failed_dep_hash : string option;
}

val append : packages_dir:string -> pkg_str:string -> entry -> unit
val read : packages_dir:string -> pkg_str:string -> entry list
val read_latest : packages_dir:string -> pkg_str:string -> entry list
val read_blessed : packages_dir:string -> pkg_str:string -> entry option
val compact : packages_dir:string -> pkg_str:string -> max_age_days:int -> unit
```

### `Status_index` — global status index

```ocaml
type change = { package : string; build_hash : string; blessed : bool; from_status : string; to_status : string }
type t = { generated : string; run_id : string; blessed_totals : ...; non_blessed_totals : ...; changes : change list; new_packages : string list }

val generate : packages_dir:string -> run_id:string -> previous:t option -> t
val write : dir:string -> t -> unit
val read : dir:string -> t option
```

### `Gc` — garbage collection

```ocaml
type layer_gc_result
type universe_gc_result

val gc_layers : ... -> layer_gc_result
val gc_universes : ... -> universe_gc_result
val gc_all : ... -> layer_gc_result * universe_gc_result
val collect_referenced_universes : html_dir:string -> string list
```

### `Build_lock` — lock tracking

```ocaml
type stage = Build | Doc | Tool
type lock_info

val list_active : cache_dir:string -> lock_info list
val cleanup_stale : cache_dir:string -> unit
```

### `Notify` — pluggable notifications

```ocaml
type channel = Slack | Zulip | Telegram | Email | Stdout
val send : channel:channel -> message:string -> int
```

### `Atomic_swap` — safe atomic directory swaps

(Note: the `exec` library has a similar `Atomic_swap` module. This one
in `lib` is the day10_lib version used by the web dashboard. Consider
consolidating.)

### `Batch_util` — pure utility functions

```ocaml
val contains_substring_ci : pattern:string -> string -> bool
val matches_any : string list -> string -> bool
val extract_compiler_from_deps : Yojson.Safe.t -> string
val classify_build_log : string -> string * string * string option
```

## Source in day10

Carried forward from `day10/lib/` with the same modules. The only
addition from the refactoring is `Batch_util` which was extracted from
`main.ml` earlier in this session.

## Notes

- `History.append` uses file locking (`Unix.lockf`) for concurrent
  access from forked workers.
- `Progress` writes JSON that the web dashboard polls.
- `Status_index` compares current run against previous to compute
  changes (new failures, recoveries, new packages).

### Eio locking

`History.append` uses a dual-layer lock: a per-path `Eio.Mutex.t`
for intra-process serialisation and `Unix.lockf` on the file for
cross-process serialisation. POSIX `lockf` is keyed by `(inode,
pid)`, so without the Eio mutex two fibers in the same process
would never block each other via the OS. Callers must be running
under an Eio scheduler (e.g. `Eio_main.run`); the lib test suite
wraps the whole run in `Eio_main.run` for this reason.

This matches the dual-layer approach in `day11_sys.Dir_lock`.

## Testing

Pure data library — all tests are unit tests with no external deps
beyond `unix` and `yojson`. Use temp dirs for isolation.

### Unit tests

- **`Run_log`** — `start_run` creates a run dir. `get_id` returns a
  non-empty string. `add_build_log` copies a file into the run dir.
  `finish_run` returns a summary with correct counts.
- **`Progress`** — `create` → `set_phase` → `to_json` round-trips.
  `write`/`delete` creates and removes the progress file. Verify
  phase transitions: `Solving → Blessings → Building → Gc →
  Completed`.
- **`History`** — `append` then `read` returns the entry. Multiple
  appends produce ordered entries. `read_latest` filters correctly.
  `read_blessed` returns only blessed entries. `compact` with
  `max_age_days:0` removes old entries.
- **`Status_index`** — `generate` with a known packages dir produces
  expected totals. Set up two runs, verify `changes` detects new
  failures and recoveries.
- **`Gc`** — create a mock cache dir with referenced and unreferenced
  layers/universes, verify `gc_layers` deletes only unreferenced ones.
  Test `collect_referenced_universes` scans html dir correctly.
- **`Build_lock`** — `list_active` on a dir with lock files returns
  them. `cleanup_stale` removes locks older than threshold.
- **`Notify`** — test `Stdout` channel (capture stdout, verify message
  appears). Other channels can be stubbed.
- **`Batch_util`** — `contains_substring_ci` with various cases.
  `matches_any` with matching/non-matching patterns.
  `classify_build_log` with known log snippets (transient failure,
  missing depext, success).

### Failure mode tests

- **`History` — corrupt JSONL:** write a valid entry then a truncated
  line (simulating a crash mid-append). Verify `read` returns the
  valid entries and skips the broken line.
- **`History` — empty file:** `read` on a 0-byte history.jsonl →
  returns `[]`, not an exception.
- **`History` — concurrent append:** two fibers (or threads) append
  to the same file simultaneously. Verify both entries appear and the
  file is not corrupted. This validates `Unix.lockf` correctness.
- **`Progress` — corrupt JSON:** `read` (if any) with a malformed
  progress.json → graceful fallback.
- **`Status_index` — missing packages dir:** `generate` when the
  packages dir doesn't exist → handles gracefully (empty totals or
  `Error`).
- **`Build_lock` — stale locks:** create lock files with old
  timestamps. Verify `cleanup_stale` removes them. Verify
  `list_active` still returns currently-valid locks.
- **`Notify` — delivery failure:** `send` to an unreachable channel
  (e.g. bad Slack webhook) → returns a non-zero exit code, not an
  uncaught exception.
