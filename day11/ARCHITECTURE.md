# day11 Architecture

day11 is a restructured version of day10, developed alongside it. The goal is
to decompose the monolithic `main.ml` (~4300 lines) and flat `bin/` namespace
into well-separated libraries with clear domain boundaries.

## Design Principles

1. **The individual package build is the core unit of work.** Everything else
   (batch orchestration, blessings, DAG scheduling) is coordination on top.

2. **Libraries should not pull in dependencies they don't need.** A debug
   session shouldn't need the solver. A build shouldn't need batch reporting.

3. **Each library has a clear "what it knows about" boundary.** The `layer`
   library knows about on-disk layer structure but not how containers work.
   The `container` library knows how to run runc but not about doc generation.

4. **Use established OCaml libraries instead of rolling our own.**
   - `bos` + `fpath` for filesystem and path operations
   - `logs` for levelled, per-source logging
   - `eio` for structured concurrency (following `odoc_driver`'s pattern)
   - `result` types for error handling throughout

## Foundation Libraries

- **`bos`** — file I/O, directory ops, path manipulation, command building.
  Used directly (not wrapped) for `OS.File.read/write`, `OS.Dir.create/delete`,
  `OS.Path.link/symlink/move`, `Bos.Cmd.t` construction.

- **`fpath`** — typed paths (`Fpath.t`) everywhere instead of bare strings.

- **`logs`** — each library creates its own `Logs.Src.t` for source-specific
  filtering. The binary installs a reporter at startup.

- **`eio`** — structured concurrency via fibers and promises. Subprocess
  execution via `Eio.Process.spawn`. Worker pool pattern from `odoc_driver`:
  N daemon fibers pulling from a shared `Eio.Stream`, promises for DAG
  ordering. Replaces all `Unix.fork`-based parallelism.

## Libraries

### `exec` — Process execution & filesystem primitives

The foundation layer. Domain-specific operations on top of Bos/Eio: sudo
execution, retry logic, worker pool, directory locking, tree operations,
atomic swaps.

**Dependencies:** `eio`, `bos`, `logs`

**See:** [exec/README.md](exec/README.md)

---

### `layer` — On-disk layer management

The layer abstraction: `layer.json` serialization, package symlinks,
installed file scanning, opam repo assembly, skeleton layers, layer
queries, build failure classification.

**Dependencies:** `exec`, `yojson`, `opam-format`

**See:** [layer/README.md](layer/README.md)

---

### `container` — Container runtime abstraction

OCI/runc lifecycle: overlay assembly, OCI spec generation, runc
execution, debug shells. Platform implementations for Linux, FreeBSD,
Windows.

**Dependencies:** `exec`, `layer`, `yojson`, `opam-format`, `dockerfile`

**See:** [container/README.md](container/README.md)

---

### `solver` — Dependency resolution

Package constraint solving, dependency graphs, topological sort. The
only library that needs `opam-0install` and `git-unix`.

**Dependencies:** `exec`, `layer`, `opam-0install`, `git-unix`

**See:** [solver/README.md](solver/README.md)

---

### `doc` — Documentation generation

odoc orchestration, doc tool layers, prep structure, combining docs,
syncing docs, deferred linking.

**Dependencies:** `exec`, `layer`, `container`, `opam-format`

**See:** [doc/README.md](doc/README.md)

---

### `jtw` — JavaScript/REPL generation

js_of_ocaml artifacts: CMI generation, findlib index, worker.js builds.

**Dependencies:** `exec`, `layer`, `container`

**See:** [jtw/README.md](jtw/README.md)

---

### `lib` — Run management & reporting

Run logging, progress tracking, build history, status index, GC,
notifications. Also used by the web dashboard.

**Dependencies:** `unix`, `yojson` (minimal — no opam, no exec)

**See:** [lib/README.md](lib/README.md)

---

### `build` — Individual package build

The core build unit: solve one target, build all deps in order, create
build/doc/jtw layers.

**Dependencies:** `exec`, `layer`, `container`, `solver`, `doc`, `jtw`

**See:** [build/README.md](build/README.md)

---

### `batch` — Multi-target batch orchestration

Blessings, Eio-based DAG execution, parallel doc generation, batch
summary, GC coordination, incremental solving.

**Dependencies:** `build`, `lib`, `solver`, `eio`

**See:** [batch/README.md](batch/README.md)

---

### `day11` (the binary) — CLI only

Argument parsing (cmdliner) and thin command dispatch. Wraps everything
in `Eio_main.run`, installs the `Logs` reporter, and dispatches to
library functions.

**Dependencies:** everything above + `cmdliner`

---

## Dependency Graph

```
exec                lib
  |                  |
layer                |
  |                  |
  +------+------+    |
  |      |      |    |
container|      |    |
  |      |      |    |
  |    doc    jtw    |
  |      |      |    |
  +--solver          |
        |            |
      build          |
        |            |
      batch----------+
        |
      day11 (binary)
```

## Workflow → Library mapping

| Workflow | Libraries needed |
|----------|-----------------|
| Build one package | `exec`, `layer`, `container`, `solver`, `build` |
| Build one package + docs | above + `doc` |
| Debug a failed build | `exec`, `layer`, `container`, `lib` (history) |
| Batch build (entire repo) | all of the above + `batch`, `lib` |
| Rerun a failed package | `exec`, `layer`, `container`, `build` |
| Cascade rebuild | `exec`, `layer`, `container`, `build`, `lib` |
| Query status/history | `exec`, `layer`, `lib` |
| View build log | `exec`, `layer` |
| GC | `exec`, `layer`, `lib` |
| Sync/combine docs | `exec`, `layer`, `doc` |

## Migration Strategy

day11 is developed alongside day10 — both coexist in the monorepo. We build
up day11 library by library, testing each against day10's existing
functionality:

1. Start with `exec` — extract the foundation from `os.ml`, add Eio worker pool
2. Then `layer` — extract layer management from `util.ml` + `main.ml`
3. Then `container` — move `s.ml`, `linux.ml`, etc. largely as-is
4. Then `solver` — move solver modules largely as-is
5. Then `doc` and `jtw` — move as-is
6. Then `build` — extract from `main.ml`'s `build`/`build_layer`
7. Then `batch` — extract from `main.ml`'s `run_batch`
8. Finally wire up the CLI

Each step should produce a building, testable library before moving to the
next.
