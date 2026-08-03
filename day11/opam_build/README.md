# opam_build — Individual opam package build

The core build unit. Given a solved dependency list, builds one package
and all its dependencies in topological order. This is what you'd use
for "build this one package" — whether interactively or as the unit of
work called by the batch orchestrator.

## External dependencies

- `exec` (locking, temp dirs, sudo)
- `layer` (layer info, opam repo, symlinks, installed files)
- `container` (runs builds in containers)
- `solver` (solve, topological sort, dependency graph)
- `doc` (doc layer generation, deferred linking)
- `jtw` (JTW layer generation)

## Key concepts

**A build is the entire dependency chain**, not just one package. When
you build `yojson.2.2.2`, you build ocaml-base-compiler, then dune,
then cppo, then ... then yojson. Each package becomes a layer that
subsequent packages stack on.

**Layers are cached by content hash.** The hash depends on the package
and all its transitive dependencies. If the layer already exists, the
build is skipped.

**Build, doc, and JTW are separate layers.** A successful build layer
can optionally produce a doc layer and/or a JTW layer. These depend on
the build layer but have their own hashes.

## Modules

### `Build_layer` — build one package in a container

```ocaml
val build_layer :
  Container.t -> OpamPackage.t -> string ->
  OpamPackage.t list -> string list ->
  Layer.build_result
(** [build_layer t pkg layer_name ordered_deps dep_hashes]
    builds [pkg] in a container with [dep_hashes] as lower layers.
    Creates layer.json, scans installed files, creates package
    symlinks. Returns Success or Failure with the layer name.
    Handles locking (only one worker builds a given layer) and
    caching (skips if layer.json already exists). *)
```

### `Doc_layer` — generate docs for one package

```ocaml
val doc_layer :
  Container.t -> OpamPackage.t -> string -> string list ->
  ocaml_version:OpamPackage.t option ->
  compiler_layers:string list ->
  ?blessed:bool -> unit ->
  string option
(** [doc_layer t pkg build_layer_name dep_doc_hashes ...]
    generates documentation. Determines whether to use Doc_all or
    Doc_compile_only based on post deps. Creates prep structure,
    runs odoc_driver_voodoo in container, saves doc layer.json.
    Returns [Some doc_layer_name] on success, [None] on failure. *)
```

### `Jtw_layer` — generate JTW artifacts for one package

```ocaml
val jtw_layer :
  Container.t -> OpamPackage.t -> string -> string list ->
  ocaml_version:OpamPackage.t option ->
  compiler_layers:string list ->
  string option
(** Similar to doc_layer but for JavaScript artifacts. *)
```

### `Hash_cache` — memoized hash computation

```ocaml
val cached_pkg_opam_hash : t:Container.t -> OpamPackage.t -> string
val cached_layer_hash_global : t:Container.t -> OpamPackage.t list -> string
```

Global caches (hashtables) for layer hashes and opam file hashes.
Avoids redundant computation when building many packages that share
dependencies.

### `Init` — base image initialization

```ocaml
val init : Container.t -> unit
(** Ensure the base container image exists. Builds via Docker if
    needed, checking/storing the base hash. *)
```

### `Build` — orchestrate a single target

```ocaml
val build :
  Config.t -> OpamPackage.t -> Layer.build_result list
(** [build config target] solves [target], then builds all packages
    in topological order. For each package:
    1. Compute layer hash from package + deps
    2. If dep failed → write skeleton layer, skip
    3. Otherwise → build_layer (container build)
    4. If build succeeded + with_doc → doc_layer
    5. If build succeeded + with_jtw → jtw_layer
    Tracks compiler_layers for tool layer stacking.
    Optionally prunes layers after doc extraction. *)
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `main.ml` | `build_layer` (lines 328-389), `doc_layer` (lines 391-479), `jtw_layer` (lines 484-531), `cached_pkg_opam_hash`, `cached_layer_hash_global`, `init` (lines 16-41), `build` (lines 565-712) |

## Notes

- `build_layer` uses `Dir_lock.with_lock` to ensure only one worker
  builds a given layer hash. Other workers block until the layer.json
  appears.
- The `compiler_layers` ref tracks which layers form the compiler stack.
  After the compiler package builds, its dep hashes + its own hash are
  captured. Tool layers (doc-driver, doc-odoc, jtw-tools) stack on
  these to avoid recompiling the compiler.
- `build` returns a list of `build_result` values. The last element is
  always `Solution solution` (or `No_solution`). Earlier elements are
  per-package results.

## Testing

### Unit tests

- **`Hash_cache`** — `cached_pkg_opam_hash` returns consistent values.
  Cache hit on second call (verify via timing or internal state).
- **`cached_layer_hash_global`** — same package list → same hash.
  Superset → different hash.

### Integration tests (needs containers + opam repos)

- **`Build_layer`** — build `ocaml-base-compiler` (the simplest real
  package). Verify layer.json exists with `exit_status=0`. Verify
  package symlink created. Verify second call is a cache hit (no
  container launched).
- **`Doc_layer`** — after a successful build, generate docs. Verify
  doc layer.json exists. Test with a package that has post deps →
  verify `Doc_compile_only` phase.
- **`Jtw_layer`** — after a successful build, generate JTW. Verify
  JTW layer.json exists.
- **`Init`** — verify base image creation is idempotent.
- **`Build.build`** — solve + build a small package (e.g.
  `seq.base`), verify all deps built, correct topological order,
  final result is `Success`.
- **Failure cascade** — build a package whose dep fails (mock or use
  a known-broken package). Verify skeleton layers written for
  dependents with `exit_status=-1`.

### Failure mode tests

- **`Build_layer` — container crash mid-build:** create a layer dir
  with `fs/` but no `layer.json` (simulating a crash between
  container exit and metadata write). Verify `build_layer` detects
  the incomplete state and rebuilds from scratch rather than treating
  it as complete.
- **`Build_layer` — concurrent workers, same hash:** two Eio fibers
  call `build_layer` for the same layer hash. Verify only one
  actually runs the container; the other waits for `layer.json` via
  `Dir_lock` / `Wait`. Verify both return `Success`.
- **`Build_layer` — permission error on layer dir:** `chmod 000` the
  cache dir, attempt `build_layer` → `Error` propagates cleanly.
- **`Doc_layer` — doc tools missing:** attempt doc generation when
  the driver layer doesn't exist. Verify `None` return (skip), not
  an exception.
- **`Doc_layer` — odoc_driver_voodoo crashes:** container exits
  non-zero during doc generation. Verify doc layer.json is written
  with failure status and no partial HTML is left behind.
- **`Build.build` — solver fails:** `build` with a package that has
  no solution → returns `[No_solution msg]`, not an exception.
- **`Init` — Docker unavailable:** `init` when Docker is not on
  `PATH` → clear `Error`.

### Fault injection

`build` orchestrates container, solver, doc, and jtw — making it
the primary consumer of injected faults from lower libraries.

- **Via `Fist_container`** (see `container` README): test the full
  `Build.build` pipeline without real containers.
  - Inject a build failure at package N in a 5-package chain →
    verify packages 1..(N-1) succeed, N fails, (N+1)..5 get
    skeleton layers with `exit_status=-1`.
  - Inject a doc failure for a package → verify build layer is
    preserved (`Success`) but doc layer.json records `Doc_failure`.
  - Inject `Signaled 9` (OOM kill) for a package → verify it's
    treated as a failure, not a crash.
- **Via injectable `Run`** (see `exec` README): test `Build_layer`
  locking behavior.
  - Inject a slow build (simulated delay) to verify that a second
    fiber blocks on `Dir_lock` and returns the cached result once
    the first fiber finishes.
- **Solver fault injection:** supply a `Dir_context` with a crafted
  opam repo where a specific package has unsatisfiable constraints.
  Verify `Build.build` returns `No_solution` cleanly.
