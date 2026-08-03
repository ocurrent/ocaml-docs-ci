# batch — Multi-target batch orchestration

The coordination layer for building entire opam repositories. Calls into
`build` for actual package builds, adds DAG scheduling, blessings,
parallel execution, and reporting.

## External dependencies

- `build` (individual package builds)
- `lib` (run logging, progress, history, status, GC, notifications)
- `solver` (solving, incremental reuse)
- `exec` (worker pool, filesystem)
- `layer` (DAG construction, layer queries)
- `container` (init/deinit, hash computation)
- `doc` (deferred linking)
- `jtw` (JTW assembly)
- `eio` (structured concurrency for parallel execution)

## Key concepts

**Batch builds many targets.** Each target is a package that gets solved
independently, producing a solution (dependency graph). Across targets,
many packages are shared — the global DAG deduplicates them.

**Blessings determine canonical docs.** When the same package appears in
multiple solutions (different universes), the blessing system picks the
"best" universe per package for canonical documentation.

**Eio-based parallel execution.** The DAG executor uses Eio fibers and
promises for dependency ordering. Each build node is a fiber that awaits
its dependency promises, submits work to the worker pool, then resolves
its own promise. This replaces the day10 fork-based DAG executor.

## Modules

### `Blessing` — version selection across universes

```ocaml
val universe_hash_of_deps : OpamPackage.Set.t -> string
val compute_blessings :
  (OpamPackage.t * OpamPackage.Set.t OpamPackage.Map.t) list ->
  (OpamPackage.t * bool OpamPackage.Map.t) list
(** Heuristic: maximize deps_count (richer docs), then revdeps_count
    (stability). Returns per-target blessing maps. *)

val is_blessed : bool OpamPackage.Map.t -> OpamPackage.t -> bool
val save_blessed_map : Fpath.t -> bool OpamPackage.Map.t -> unit
val load_blessed_map : Fpath.t -> bool OpamPackage.Map.t
```

### `Dag_executor` — Eio-based parallel DAG execution

```ocaml
val execute :
  env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t -> np:int ->
  cache_dir:Fpath.t -> os_key:string ->
  on_complete:(total:int -> completed:int -> failed:int -> string -> bool -> unit) ->
  on_cascade:(failed_hash:string -> failed_dep_hash:string -> unit) ->
  Layer.build_node list -> (Layer.build_node -> bool) -> unit
(** Each build node becomes an Eio fiber. The fiber awaits promises for
    all its dependencies, then submits the build to the worker pool.
    On completion it resolves its own promise, unblocking dependents.
    Failures cascade: if a dep fails, the dependent's promise is
    resolved with failure without building. *)
```

This replaces day10's 140-line `execute_dag` (IntSet PID tracking,
waitpid loops, ready queues) with ~30 lines of Eio fiber code.

### `Doc_layers` — parallel doc/jtw generation

```ocaml
val run :
  env:Eio_unix.Stdenv.base -> sw:Eio.Switch.t ->
  config:Config.t -> np:int -> dag_nodes:Layer.build_node list ->
  solutions:... -> per_solution_hashes:... -> blessing_maps:... ->
  build_success_set:(string, bool) Hashtbl.t ->
  node_by_hash:(string, Layer.build_node) Hashtbl.t ->
  packages_dir:Fpath.t -> run_id:string ->
  (string * string) list
(** Parallel doc/jtw generation using Eio fibers with DAG ordering.
    Same pattern as Dag_executor: each node awaits dep doc promises
    before generating its own docs. Returns (pkg_str, doc_layer_name)
    pairs. *)
```

### `Summary` — result aggregation and reporting

```ocaml
val print :
  config:Config.t -> solutions:... -> blessing_maps:... ->
  num_packages:int -> total_failed:int -> run_info:Lib.Run_log.t ->
  ?per_solution_hashes:... -> ?doc_layers:(string * string) list ->
  unit -> unit
(** Record build/doc results in history, write universes, generate
    status.json, and print summary to stdout. *)
```

### `Jtw_assembly` — JTW output assembly

```ocaml
val assemble :
  config:Config.t -> solutions:... -> blessing_maps:... -> unit
(** Build per-solution worker.js files and assemble JTW output. *)
```

### `Gc` — garbage collection coordination

```ocaml
val run :
  config:Config.t -> solutions:... ->
  ?known_layers:string list -> unit -> unit
```

### `Incremental_solver` — solution caching and reuse

```ocaml
val reuse_solutions :
  config:Config.t -> solutions_cache_dir:Fpath.t ->
  opam_repo_sha:string -> opam_repo_full_shas:string list ->
  packages:string list -> int
(** Hardlink unchanged solutions from previous opam-repo SHA. *)
```

## The `run_batch` orchestrator

After extraction, `run_batch` becomes a thin orchestrator:

```
1. Parse packages, init logging, save build config, cleanup stale state
2. Phase 1: Solve all targets
   - Incremental_solver.reuse_solutions
   - Eio.Fiber.List.map solve_one packages  (* parallel solving *)
3. Phase 2: Compute blessings
4. Phase 3: Build
   - Dag_executor.execute (Eio fibers + worker pool)
   - Doc_layers.run (Eio fibers + worker pool)
5. Phase 4: JTW assembly
6. GC
7. Summary
8. Cleanup progress
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `blessing.ml` | `Blessing` module (127 lines) |
| `main.ml` | `execute_dag` → `Dag_executor`, `run_fork_doc_layers` → `Doc_layers`, `print_batch_summary` → `Summary`, `assemble_jtw_output` → `Jtw_assembly`, `run_gc` / `collect_referenced_layer_names` → `Gc`, incremental solver logic → `Incremental_solver`, `run_batch` orchestrator |

## Notes

- The parallel solving phase uses `Eio.Fiber.List.map` instead of
  `Os.fork_map`. No temp file serialization needed — fibers share memory.
- `Dag_executor` uses the same `Worker_pool` from `exec` that individual
  builds use. The pool is started once at the beginning of the batch.
- Cancellation: if we want to abort on first failure (optional), Eio's
  `Switch.fail` provides clean cancellation of all fibers.

## Testing

### Unit tests

- **`Blessing.compute_blessings`** — 3 targets sharing packages,
  verify the "richest universe" heuristic picks correctly. Verify a
  package in only one universe is always blessed. Test tie-breaking
  by revdeps count.
- **`Blessing` round-trip** — `save_blessed_map` / `load_blessed_map`.
- **`Incremental_solver.reuse_solutions`** — set up a solutions cache
  with known SHAs, verify hardlinks are created for unchanged
  packages.

### Integration tests (needs Eio + containers)

- **`Dag_executor`** — construct a 4-node diamond DAG (A→B, A→C, B→D,
  C→D). Execute with 2 workers. Verify all nodes complete. Verify D
  runs before B and C. Test failure cascade: if D fails, B and C get
  cascade failure.
- **`Doc_layers.run`** — after a batch build, run parallel doc
  generation. Verify doc layers created for successful builds.
- **`Summary.print`** — verify it writes history entries,
  status.json, and universe files.
- **`Gc.run`** — after a batch, add some orphan layers, run GC,
  verify only orphans removed.

### Failure mode tests

- **`Dag_executor` — fiber cancellation:** cancel the Eio switch
  mid-execution (simulating Ctrl-C). Verify no zombie runc processes
  and no half-written layer.json files. All overlay mounts should be
  cleaned up.
- **`Dag_executor` — all targets fail:** every build in the DAG
  fails. Verify `on_complete` is called for each with failure status,
  `on_cascade` is called for all dependents, and the executor returns
  cleanly.
- **`Doc_layers` — partial doc failures:** some packages succeed,
  some fail doc generation. Verify successful doc layers are
  preserved and the return list contains only successes.
- **`Incremental_solver` — stale cache:** solutions cache from a
  different opam-repo SHA (nothing reusable). Verify
  `reuse_solutions` returns 0 and doesn't corrupt the target dir.
- **`Incremental_solver` — missing cache dir:** cache dir doesn't
  exist → returns 0 (no solutions reused), not an exception.
- **`Gc` — concurrent GC and build:** start a GC while a build is
  in progress. Verify the GC does not remove layers referenced by
  active build locks.
- **`Blessing` — single-universe packages:** every package appears in
  exactly one universe. Verify all are blessed (no "unblessed"
  packages in a single-universe batch).

### Fault injection

`batch` is the top-level orchestrator, so it benefits most from
fault injection in the layers below. Use `Fist_container` + an
injectable `Worker_pool` (see `exec` and `container` READMEs) to
run the full batch pipeline as a unit test.

- **`Dag_executor` with injected failures:** configure
  `Fist_container` so specific packages fail. Verify:
  - `on_complete` is called with failure for the injected package.
  - `on_cascade` is called for all transitive dependents.
  - Successful packages are unaffected.
  - The executor completes (doesn't hang on failed promises).
- **`Doc_layers` with partial doc failures:** configure
  `Fist_container.generate_docs` to return `None` for specific
  packages. Verify the return list contains only successes and
  `History` entries record the failures.
- **Mixed solve outcomes:** provide a set of targets where some
  solve successfully and some are unsatisfiable (via crafted opam
  repos). Verify the batch continues with the solvable targets
  and reports the failures in the summary.
- **`Gc` with injected active locks:** create mock lock files (via
  `Build_lock`) before running `Gc.run`. Verify referenced layers
  survive.
- **Full pipeline under `Fist_container`:** run the complete
  solve → bless → build → docs → jtw → summary → GC pipeline with
  `Fist_container` configured to fail 1 out of 5 packages. Verify
  end state: 4 successful layers, 1 failure + its cascade
  dependents marked, history populated correctly, status index
  shows the failure. This runs in the unit test tier.

### End-to-end test

- Small batch (3 targets with overlapping deps), full pipeline:
  solve → bless → build → docs → jtw → summary → GC. Verify final
  state: all layers exist, blessings correct, history populated,
  status index generated.
