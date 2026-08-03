# day11 Testing — Cross-cutting concerns

Per-library test plans (unit tests, integration tests, failure mode
tests) live in each library's `README.md`. This document covers
failure modes and testing strategies that span multiple libraries.

## Fault injection strategy

The day11 architecture has two natural injection boundaries that,
together, let the full pipeline run as a unit test without
containers, root, or Docker.

### Injection boundary 1: `Run` (exec)

`Run.run` is the single point where day11 spawns subprocesses. By
parameterizing it (or providing a test double), higher libraries can
control what every subprocess "returns" — exit status, stdout,
stderr, timing — without actually executing anything.

**What this unlocks:** `Retry` exhaustion tests, `Worker_pool`
resilience tests, `Dir_lock` contention with simulated slow builds,
and any test that needs a specific subprocess outcome.

See `exec/README.md` → Fault injection for details.

### Injection boundary 2: `CONTAINER` module type (container)

The `CONTAINER` module type already abstracts over platform
implementations (Linux, FreeBSD, Windows, Dummy). A
`Fist_container` test implementation extends this pattern: each
method's behavior is controlled by a mutable config table. Tests
configure which packages fail `build`, `generate_docs`, or
`generate_jtw`, and with what results.

**What this unlocks:** the entire `build` and `batch` pipeline —
solving, DAG execution, doc generation, JTW generation, failure
cascades, blessings, GC, and summary — can run end-to-end under
`Fist_container` in the unit test tier, on any CI runner.

See `container/README.md` → Fault injection for the `Fist_container`
spec, and `build/README.md`, `batch/README.md`, `doc/README.md`,
`jtw/README.md` for how each library uses it.

### Injection patterns

| Pattern | Where | What it tests |
|---------|-------|---------------|
| `Fist_container` returns non-zero exit | `build`, `batch` | Failure cascade, skeleton layers, history recording |
| `Fist_container` returns `Signaled 9` | `build` | OOM handling (treated as failure, not crash) |
| `Fist_container.generate_docs` → `None` | `doc`, `build`, `batch` | Doc failure recording, deferred link skipping |
| `Fist_container.generate_jtw` → `None` | `jtw`, `build`, `batch` | JTW skip, output assembly with gaps |
| `Fist_container.init` → failure | `build` | Early abort before any builds |
| Injectable `Run` → always fails | `exec` | Retry exhaustion, worker pool resilience |
| Injectable `Run` → slow | `exec`, `build` | Lock contention, timeout behavior |
| Crafted opam repo (unsatisfiable) | `solver`, `build`, `batch` | No-solution handling, partial batch continues |
| Missing tool layer on disk | `doc`, `jtw` | Skip-cleanly path for missing doc/jtw tools |
| `chmod 000` on directories | `exec`, `layer` | Permission error propagation |

### Design guidelines

- **Keep injection test-only.** Don't add injection hooks to
  production code paths. Use OCaml's module system: tests
  instantiate `Build.Make(Fist_container)` while production uses
  `Build.Make(Linux)`. If the code isn't already functorized,
  consider accepting the container as a first-class module value.
- **Inject at the highest useful boundary.** If you can test the
  behavior via `Fist_container`, don't also inject at the `Run`
  level — that's redundant. Reserve `Run`-level injection for
  `exec`'s own tests.
- **Make injected failures deterministic.** `Fist_container` config
  should be keyed by package name (not random), so tests are
  reproducible.

## Disk space exhaustion

Disk-full is hard to simulate reliably and doesn't justify dedicated
test infrastructure. Instead, verify the following structural
properties hold across the codebase:

1. **All file writes are atomic.** `OS.File.write` uses tmp+rename
   internally, so a disk-full error leaves the original file
   untouched. Audit: grep for direct `open_out`/`output_string`
   patterns — these should be replaced with `OS.File.write` or
   wrapped in try/finally cleanup.

2. **All multi-step writes have rollback.** `Atomic_swap` (in `exec`)
   provides prepare/commit/rollback. `Layer_info.save` writes to
   a single file. `History.append` is append-only JSONL with lockf.
   Verify there's no write path that creates directory A, then
   writes file B, where a failure between A and B leaves orphaned
   state.

3. **One targeted test:** mount a `tmpfs` with `size=4k`, fill it,
   then attempt `OS.File.write` → verify the target file is
   untouched and the error propagates as `Error`.

**Libraries affected:** `exec`, `layer`, `lib`, `build`

## Error propagation audit

Every `result`-returning function in day11 should propagate errors,
never swallow them. Before each library ships, run this audit:

```
# Find potential result-swallowing patterns
grep -rn 'Result.get_ok\|ignore.*result\||> ok\b' lib/
grep -rn 'try.*with _\|with _ ->' lib/
```

Violations to flag:
- `|> Result.get_ok` — turns recoverable errors into crashes
- `try ... with _ -> ()` — silently swallows exceptions
- `ignore (some_result_fn ...)` — discards error information

**Rule:** `Result.get_ok` is acceptable only in test code. In
library code, all results must be bound with `>>=`, `let*`, or
explicit match.

**Libraries affected:** all

## Ctrl-C / SIGTERM resilience

A user pressing Ctrl-C during any operation should not leave:
- Leaked overlay mounts (stale entries in `/proc/mounts`)
- Zombie runc processes
- Half-written `layer.json` files (should be absent or complete)
- Stale lock files (should be cleaned up on next run)
- Orphaned staging directories

### How to test

Use Eio's `Switch.fail` to simulate cancellation at various points:

1. **During `Worker_pool` job execution** (`exec`): cancel the
   switch, verify the subprocess is killed and the pool shuts down.
2. **During `Dag_executor.execute`** (`batch`): cancel mid-DAG,
   verify all in-flight fibers are cancelled, no zombie processes.
3. **During `Linux.build`** (`container`): cancel between overlay
   mount and runc run, verify the mount is cleaned up.

For real-world validation, run a batch build and send `SIGTERM` at
random intervals. Check:
```bash
# No leaked mounts
mount | grep overlay | grep day11
# No zombie runc
ps aux | grep 'runc.*day11'
# No stale locks (after restart)
find $CACHE_DIR -name '.lock' -mmin +5
```

**Libraries affected:** `exec`, `container`, `build`, `batch`

## Network failures

Network is used in two places: `solver` (git fetch for incremental
solving) and `doc` (rsync for doc distribution). These are tested
per-library. The cross-cutting concern is:

**Network failures must not corrupt local state.** Specifically:
- A failed `git fetch` in `solver` must not leave a partial pack
  file in the git store. Verify by killing the fetch mid-transfer
  and checking that the store is still valid.
- A failed `rsync` in `doc` must not leave partial files at the
  destination. rsync's `--partial-dir` or `--delay-updates` flags
  handle this, but verify the flags are actually passed.

### How to test

Use `unshare --net` (Linux) to create a network namespace with no
connectivity:
```bash
unshare --net -- day11 solve <package> --git-repo <remote>
# Should fail with a clear network error, not corrupt local state
```

**Libraries affected:** `solver`, `doc`

## Concurrent access safety

### The `lockf` + Eio problem

day10 uses `Unix.lockf` for cross-process coordination between
forked workers. In day11, workers are Eio fibers in a single OS
process. POSIX `lockf` is keyed by `(inode, pid)` — **two fibers
in the same process will not block each other** because they share
a PID. This affects:

| Resource | day10 mechanism | day11 fix needed |
|----------|----------------|-----------------|
| `Dir_lock.with_lock` | `Unix.lockf` | Dual-layer: `Eio.Mutex` (intra-process) + `lockf` (cross-process) |
| `History.append` | `Unix.lockf` | Either `Eio.Mutex` or rely on cooperative scheduling (see `lib` README) |

Things that are **fine as-is:**
- **In-memory DAG state** (Hashtbls in `batch`): Eio fibers are
  cooperative; hashtable ops between yield points are atomic.
- **`Safe_rename`**: uses marker files on the filesystem, not lockf.
- **`Atomic_swap`**: uses directory rename, OS-level atomic.

See `exec/README.md` → `Dir_lock` and `lib/README.md` → Eio
locking note for implementation details.

### Testing the dual-layer lock

This is a critical correctness property. Tests should verify both
layers independently:

1. **Intra-process (Eio fiber) test:** spawn two fibers that both
   call `Dir_lock.with_lock` on the same path. Verify only one
   enters the critical section at a time (use a shared counter +
   `Eio.Fiber.yield` inside the body to force interleaving).
2. **Cross-process test:** spawn two OS-level day11 processes
   (`Unix.create_process` or `Eio.Process.spawn`) targeting the same
   layer hash. Verify only one container runs, both succeed, and
   layer.json is written exactly once.
3. **Mixed test:** one day11 process is building; a second process
   starts and tries the same layer. Verify the second process
   blocks on `lockf` (not just the Eio mutex, which is per-process).

### Other cross-process scenarios

Multiple day11 processes can run against the same cache directory
(e.g. two batch builds, or a build + a debug session). The locking
strategy:

| Resource | Lock mechanism | Library |
|----------|---------------|---------|
| Layer dir being built | `Eio.Mutex` + `Unix.lockf` | `exec` |
| History file | `Eio.Mutex` (or cooperative) + `Unix.lockf` | `lib` |
| Layer rename | `Safe_rename.dir` marker | `exec` |
| Tool layer build | `Dir_lock.with_lock` | `build` |

Run `day11 build <pkg>` and `day11 gc` simultaneously. Verify:
- GC does not remove layers referenced by active build locks
- The build completes successfully

**Libraries affected:** `exec`, `lib`, `build`, `batch`

## CI test tiers

| Tier | What runs | Environment | When |
|------|-----------|-------------|------|
| **unit** | All unit tests + failure mode tests that don't need root/containers | Any Linux CI runner | Every PR |
| **integration** | Container builds, overlay mounts, runc execution | Linux runner with runc + sudo | Nightly or on-demand |
| **stress** | Ctrl-C resilience, cross-process concurrency, disk-full | Dedicated runner | Weekly or pre-release |

Tag tests with `(* alcotest tier *)` or use dune test stanzas with
`(enabled_if ...)` to gate integration tests on an environment
variable:
```
(test
 (name test_container_integration)
 (enabled_if (= %{env:DAY11_INTEGRATION=false} true))
 (libraries day11_container alcotest))
```

## Test infrastructure

- **Framework:** alcotest — integrates with dune, supports test
  grouping by module.
- **Temp dirs:** use `OS.Dir.tmp` (auto-cleanup) for unit tests.
  For integration tests that need explicit lifetime (e.g. layer dirs
  that get renamed), create under a test-specific tmp root and clean
  up in test teardown.
- **Eio harness:** wrap every test that touches `exec`, `batch`, or
  `build` in `Eio_main.run @@ fun env -> ...`.
- **Mock opam repos:** create minimal repos (2-3 packages with
  simple deps) in temp dirs. Keep them version-controlled under
  `day11/test/fixtures/` for reproducibility.
- **Race testing:** use Eio fibers with `Eio.Fiber.yield` at
  strategic points to force deterministic interleaving. Prefer this
  over real threads — it makes race tests reproducible.
