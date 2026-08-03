# exec — Process execution & filesystem primitives

The foundation layer. No domain knowledge, no opam dependencies.

Everything else in day11 depends on this library for operations that go
beyond what `bos` provides out of the box.

## External dependencies

- `eio` / `eio_main` (structured concurrency, subprocess management)
- `bos` (and transitively `fpath`, `rresult`)
- `logs`

We use `Fpath.t` for typed paths, `('a, 'e) result` for error handling,
`Logs` for levelled logging, and `Eio` for concurrency throughout day11.

## What comes from Bos directly

These Bos APIs are used directly by day11 libraries — not wrapped.

- **File I/O** — `OS.File.read`, `OS.File.write` (atomic writes)
- **Directories** — `OS.Dir.create`, `OS.Dir.delete`, `OS.Dir.contents`,
  `OS.Dir.fold_contents`, `OS.Dir.tmp`
- **Paths** — `OS.Path.move`, `OS.Path.link`, `OS.Path.symlink`,
  `OS.Path.symlink_target`, `OS.Path.stat`, `OS.Path.delete`
- **Command building** — `Bos.Cmd.(v "prog" % "arg1" % "arg2")`
- **Typed paths** — `Fpath.t`, `Fpath.(/)`, `Fpath.v`

Note: for subprocess execution we use `Eio.Process` rather than
`Bos.OS.Cmd`, since we're in an Eio event loop. Bos is still used for
`Bos.Cmd.t` command construction and for synchronous filesystem ops.

## What this library provides on top of Bos/Eio

### `Run` — subprocess execution via Eio

Modelled on odoc_driver's `run.ml`. Launches subprocesses with proper
stdout/stderr capture via Eio pipes.

```ocaml
type t = {
  cmd : string list;
  time : float;
  output_file : Fpath.t option;
  output : string;
  errors : string;
  status : [ `Exited of int | `Signaled of int ];
}

val run : Eio_unix.Stdenv.base -> Bos.Cmd.t -> Fpath.t option -> t
(** [run env cmd output_file] spawns [cmd] as a subprocess, captures
    stdout and stderr concurrently via [Eio.Fiber.pair], and awaits
    completion. *)
```

### `Worker_pool` — Eio-based worker pool

Modelled on odoc_driver's `worker_pool.ml`. A pool of daemon fibers
pulling jobs from a shared unbuffered stream.

```ocaml
val start_workers : Eio_unix.Stdenv.base -> Eio.Switch.t -> int -> unit
(** [start_workers env sw n] spawns [n] daemon fibers that pull jobs
    from the shared stream and execute them. *)

val submit : string -> Bos.Cmd.t -> Fpath.t option -> (Run.t, exn) result
(** [submit description cmd output_file] submits a job to the pool
    and blocks (via [Eio.Promise.await]) until a worker completes it.
    Returns the run result or an exception. *)
```

Reference: `odoc/src/driver/worker_pool.ml` (53 lines).

### `Sudo` — privileged execution

Container builds produce root-owned files. We need sudo for cleanup and
overlay mounting.

```ocaml
val run : Eio_unix.Stdenv.base -> Bos.Cmd.t -> (Run.t, [> Rresult.R.msg]) result
val rm_rf : Eio_unix.Stdenv.base -> Fpath.t -> (unit, [> Rresult.R.msg]) result
(** Tries without sudo first, falls back on EACCES/EPERM. *)
```

### `Retry` — retried operations

```ocaml
val exec : ?tries:int -> Eio_unix.Stdenv.base -> Bos.Cmd.t -> (unit, [> Rresult.R.msg]) result
val rename : ?tries:int -> Fpath.t -> Fpath.t -> (unit, [> Rresult.R.msg]) result
```

### `Safe_rename` — race-aware directory placement

```ocaml
val dir : marker_file:Fpath.t -> Fpath.t -> Fpath.t -> (unit, [> Rresult.R.msg]) result
(** Handles races where another worker completed, or stale dirs from crashes. *)
```

### `Dir_lock` — directory-level locking

```ocaml
val with_lock :
  ?marker_file:Fpath.t -> ?lock_file:Fpath.t -> Fpath.t ->
  (set_temp_log_path:(Fpath.t -> unit) -> Fpath.t -> (unit, [`Msg of string]) result) ->
  (unit, [`Msg of string]) result
```

This module has no domain knowledge. The caller determines the lock
file path (e.g. under a `locks/` directory with a name derived from
package/version/universe). If `lock_file` is not provided, it
defaults to `dir_path ^ ".lock"`.

**Eio locking note:** day10 used `Unix.lockf` exclusively, which
works across forked processes. In day11, all workers are Eio fibers
in a single OS process. POSIX `lockf` is keyed by `(inode, pid)` —
two fibers in the same process won't block each other. `Dir_lock`
therefore needs **two layers of locking:**

1. **Intra-process (Eio fibers):** a hashtable of `Eio.Mutex.t`
   values keyed by lock path. Before touching the file lock, a
   fiber acquires the in-memory mutex. This serializes fibers within
   one day11 process.
2. **Cross-process (separate day11 invocations):** keep `Unix.lockf`
   on the lock file for the case where two day11 processes share
   the same cache directory.

The implementation acquires the Eio mutex first, then the file lock.
On release, the file lock is released first, then the Eio mutex.
The Eio mutex table can be a process-global `(string, Eio.Mutex.t)
Hashtbl.t` — since Eio is cooperative, hashtable access between
yield points is safe without additional synchronization.

### `Tree` — recursive directory tree operations

```ocaml
val hardlink : source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg]) result
(** Hardlink tree; falls back to copy on EMLINK. *)

val clense : source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg]) result
(** Remove files from target that are identical to source (same mtime).
    Used after overlay to extract only new/changed files. *)

val copy : source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg]) result
```

### `Atomic_swap` — atomic directory replacement

This module has no domain knowledge. The caller computes the target
path from their own domain types (e.g. `html_dir/p/<pkg>/<version>`
for blessed docs). The module manages the staging (`.new`) and
backup (`.old`) directories relative to the target.

```ocaml
val cleanup_stale : Eio_unix.Stdenv.base -> Fpath.t -> (unit, [> Rresult.R.msg]) result
val prepare : Fpath.t -> (Fpath.t, [> Rresult.R.msg]) result
val commit : Eio_unix.Stdenv.base -> Fpath.t -> (bool, [> Rresult.R.msg]) result
val rollback : Eio_unix.Stdenv.base -> Fpath.t -> (unit, [> Rresult.R.msg]) result
```

`cleanup_stale`, `commit`, and `rollback` take `env` because they
may need to call `Sudo.rm_rf` on root-owned directories left by
container builds. `prepare` does not need sudo — it only creates
user-owned directories.

### `Reporter` — per-PID file logging reporter

A `Logs.reporter` that writes to per-PID log files with millisecond
timestamps. Installed by the binary at startup.

```ocaml
val pid_file_reporter : dir:Fpath.t -> Logs.reporter
```

### Misc

```ocaml
val nproc : unit -> int
val dir_size : Fpath.t -> int
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `os.ml` | Everything except layer info serialization |
| `path.ml` | Replaced by `Fpath` |

## Notes

- The binary's `main` wraps everything in `Eio_main.run @@ fun env -> ...`
- `Eio_unix.Stdenv.base` (or the process manager from it) gets threaded
  to functions that need to spawn subprocesses.
- `Bos.OS.Cmd` is NOT used for command execution — we use `Eio.Process`
  instead. Bos is still used for `Bos.Cmd.t` construction and for
  synchronous filesystem ops that don't need Eio's scheduler.
- `OS.Dir.tmp` auto-deletes at exit; we need explicit lifetime management
  for build temps that get renamed to layer dirs.
- The old `Fork` module (`fork`, `fork_with_progress`, `fork_map`) is
  entirely replaced by `Worker_pool` + `Eio.Fiber.List.map`.

## Testing

All tests run inside `Eio_main.run` and use `OS.Dir.tmp` for isolation.

### Unit tests

- **`Run`** — spawn `echo hello`, verify `t.output`, `t.errors`,
  `t.status = Exited 0`, `t.time > 0`. Spawn `false`, verify
  `Exited 1`. Test `output_file` captures stdout to disk.
- **`Worker_pool`** — start 2 workers, submit 4 jobs, assert all
  complete. Verify concurrency: 4 sleep-0.1s jobs on 2 workers should
  finish in ~0.2s not ~0.4s.
- **`Retry`** — mock a command that fails N-1 times then succeeds;
  confirm `exec ~tries:N` succeeds and `exec ~tries:(N-1)` fails.
  Test `rename` retry with a file that temporarily doesn't exist.
- **`Tree.hardlink`** — create a temp tree, hardlink it, verify inodes
  match. Test `copy` as the EMLINK fallback path.
- **`Tree.clense`** — create source and target with identical +
  different files, verify only different files survive in target.
- **`Tree.copy`** — copy a nested directory, verify contents match.
- **`Safe_rename`** — happy path (rename succeeds), race case (target
  already exists with marker), stale case (target exists without
  marker → remove + retry).
- **`Dir_lock`** — acquire a lock, verify a second `with_lock` on the
  same dir blocks (use two Eio fibers). Verify lock release on normal
  return and on error.
- **`Atomic_swap`** — `prepare_staging` → write files → `commit` →
  verify files at final path. Test `rollback` cleans up staging.
  Test `cleanup_stale` removes old staging dirs.
- **`Reporter`** — install `pid_file_reporter`, emit a log message,
  verify the per-PID log file contains the message with a timestamp.
- **`nproc`** — returns a positive int.
- **`dir_size`** — create a temp dir with known-size files, verify
  returned size.

### Failure mode tests

- **`Run` — subprocess signals:** spawn `sleep 3600`, cancel the Eio
  fiber, verify `t.status = Signaled _` and that no zombie process
  remains. This is the "runc hangs" recovery path.
- **`Run` — nonexistent binary:** `run` with a command that doesn't
  exist on `PATH`, verify `Error` with a clear message (not an
  uncaught `Unix_error`).
- **`Worker_pool` — job exception:** submit a job that raises, verify
  the pool stays alive and subsequent jobs succeed. One bad job must
  not poison the pool.
- **`Worker_pool` — fiber cancellation:** cancel the switch mid-batch,
  verify no zombie subprocesses and no half-written output files.
- **`Retry` — exhaustion:** `exec ~tries:3` with a command that
  always fails. Verify exactly 3 attempts (count via side-effect
  file), then `Error`.
- **`Tree` — permission denied:** `chmod 000` a subdirectory in the
  source tree, attempt `hardlink`/`copy`, verify `Error` propagates
  (not an uncaught exception).
- **`Tree` — cross-filesystem:** `hardlink` across mount points
  (EXDEV), verify it falls back to `copy` automatically.
- **`Dir_lock` — body raises:** acquire a lock, raise inside the
  body, verify the lock file is cleaned up.
- **`Dir_lock` — stale lock:** create a lock file manually (simulating
  a crashed process), verify a new `with_lock` can detect and break
  the stale lock.
- **`Safe_rename` — target exists, no marker:** simulate a stale dir
  from a crash (target exists but marker file is absent), verify
  `Safe_rename.dir` removes it and retries.
- **`Atomic_swap` — stale staging:** create `pkg.staging-*` dirs from
  a "previous crash", call `cleanup_stale`, verify they're removed
  and the original data is untouched.
- **`Atomic_swap` — rollback:** call `prepare_staging`, write partial
  data, call `rollback`, verify staging dir is gone.

### Fault injection

`Run` and `Worker_pool` are the subprocess boundary that all higher
libraries call through. Providing an injectable `Run` enables the
rest of day11 to test error handling without real containers.

- **Injectable `Run`:** parameterize `Run.run` (or provide an
  alternative signature) so that tests can supply a function
  `Bos.Cmd.t -> Run.t` instead of actually spawning a process. The
  fake can return any combination of exit status, stdout, stderr,
  and timing. This lets `build`, `container`, `doc`, and `jtw`
  tests simulate:
  - Build failures (non-zero exit)
  - Segfaults (`Signaled 11`)
  - Slow commands (high `time` field)
  - Garbage on stderr
- **Injectable `Worker_pool`:** same idea — accept a job handler
  function so tests can control which jobs fail, how long they take,
  and in what order they complete. Useful for `batch` DAG executor
  tests without real builds.
- **Filesystem fault injection for `Tree`/`Atomic_swap`:** use
  `chmod 000` on subdirectories to simulate permission errors at
  specific points in a tree walk. Verify errors propagate and
  partial state is cleaned up.

### Integration tests (needs root — skip in CI)

- **`Sudo`** — verify `rm_rf` on a root-owned temp dir works, and
  that it tries non-sudo first on user-owned dirs.
- **`Sudo` — unavailable:** set `PATH` to exclude sudo, attempt
  `rm_rf` on a root-owned dir, verify `Error` (not a hang or
  uncaught exception).
