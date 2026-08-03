# Plan: per-universe doc nodes

## The bug

`odoc.3.2.1` never gets a blessed doc build and renders no HTML; single-universe
packages (`octavius.1.2.2`) work. Root cause: docs are keyed off **build nodes**,
which collapse multiple doc-universes into one.

- A *universe* `U = of_deps(transitive doc_deps closure of pkg)`
  (`Day11_solution.Universe.of_deps`).
- `batch/blessing.ml` picks, per package, the universe with the most doc-deps and
  records `pkg -> blessed_universe`. For odoc it picks the 97-dep universe
  (verified: odoc appears in 216 solutions across 10 distinct doc-universes,
  sizes 93–97).
- `opam_build/dag.ml build_dag` dedups build nodes by **build-deps** layer hash and
  stamps each with only the *first-touching* solution's universe. odoc collapses to
  4 build nodes carrying 96-dep (etc.) universes; the 97-dep universe won only 11 of
  ~193 solutions, so no surviving node carries it.
- `doc/generate.ml` blesses a node only when `Universe.equal node.universe blessed_u`
  — never true for odoc → nothing blessed → no rendered docs.

Build-layer dedup is **correct** (the opam install is byte-identical across
doc-universes sharing a build closure). The bug is that the *doc* pipeline collapses
universes onto build nodes.

## Universe relationship (settled)

- `U = of_deps(build-deps ∪ doc-only-extras)` transitively (extras = `{post}` +
  `x-extra-doc-deps`).
- build hash = `f(build-deps closure)` — insensitive to the doc-only extras, **not**
  to build-deps (which are part of `U`).
- **bh → U is one-to-many** (one build layer serves universes differing only in
  extras). **U → bh is one-to-one** (a universe pins one build closure).
- Therefore there is no `pkg → bh` function (odoc has 4 build hashes). The `(bh, U)`
  pair is derived together per `(solution, pkg)`.

## Target model

| node | keyed by | per-universe? |
|---|---|---|
| build | `build_hash` | no — shared opam install |
| compile / doc-all | `(build_hash, U)` | yes — output namespaced `prep/universes/<U>/…` |
| link | `(build_hash, U)` | yes — mounts that universe's exact closure |

The single universe notion `U = of_deps(doc_deps closure)` is threaded everywhere,
replacing the two stale relabels (`compute_universe_hash[build_hash]` in
`doc_build.prepare` and in `generate.ml` meta/dag.json).

We materialize compile+link for **every** universe a package appears in (cross-refs
go any-to-any within a solution). Only the blessed universe is rendered/served to
HTML, but all universes' layers get built. No cap (confirmed acceptable).

## Resolved questions

- **D.** `Command.odoc_driver_voodoo` ignores `~universe`; voodoo derives the served
  path solely from the prep-dir universe. So the served `u/<U>/<pkg>` path uses `U`.
  `sync.ml`/`combine.ml`/`odoc_store` must stop recomputing
  `compute_universe_hash[dep_doc_hashes]` and read `U` from dag.json by **doc-layer
  hash**.
- **C.** `src/web/pages.ml` must join history→dag.json by **doc-layer hash** (a build
  hash now appears under several universes; the doc-layer hash is unique per `(bh,U)`).
- **E.** Derive `(bh, U)` together per `(solution, pkg)` — no `pkg → bh` index. Get
  `bh` either by reusing `dag.build_dag`'s `Hash_cache`/`base_hash`, or by a
  `build_closure_set → bh` map reconstructed from existing dag nodes (each node's
  transitive `.deps` is its build closure).
- **F.** No cap on per-universe fan-out.

## Steps

0. **Prep / no change.** Canonical `U = of_deps(transitive_deps doc_deps)[pkg]` — same
   value `dag.ml` already stamps for the first-touching solution.

1. **`doc_build.prepare` takes `~universe:U`** (delete the
   `compute_universe_hash[build_layer]` derivation); thread `~universe` through
   `run_doc_phase`/`compile`/`link`/`doc_all` and the `generate.ml` `*_package`
   wrappers; `make_dispatch` passes `dn.universe`. **Behavioral no-op today** (the
   value passed equals what `prepare` computed), so it lands first and standalone.

2. **Fold `U` into layer hashes.** `compute_compile_hash ~universe`, memoised by
   `(hash, U)`, fold `U` in, bump `"v3"→"v4"`. Link hash folds `U`, bump `"v2"→"v3"`.
   (Lands with Step 3 — needs a real `U` supplied.)

3. **Solution-driven, `(bh,U)`-keyed doc-node construction** (the big one). Enumerate
   `(bh, U, closure)` over `solutions`. Re-key `compile_nodes`, `doc_all_nodes`,
   `doc_node_by_build`, `doc_node_cache`, `compile_hash_cache`, `doc_dep_hashes` to
   `(bh, U)`. `make_doc_node ~universe` resolves each dep to *its own* universe within
   the same solution. `doc_node.universe := U` (not the relabel). `blessed :=
   Universe.equal U blessed_universe[pkg]`. Drop the `compiler_s` union hack in
   `doc_dep_hashes`. `meta` stays keyed by each node's own *layer* hash (distinct per
   `(bh,U)` once Step 2 lands).

4. **`make_dispatch`** passes `~universe:dn.universe` to the doc builders.

5. **Per-node blessing into the recorder.** `Recorder.is_blessed` is package-level;
   `cmd_batch`/`cmd_build` `on_doc_complete` must read per-node blessing from the
   plan/meta, else every universe's layer records `blessed=true` and `status.json`
   totals are wrong.

6. **Version bumps.** compile/link hashes (above) + `epoch.ml` `v1→v2`. Invalidates
   all **doc** layers → full doc rebuild; **build layers untouched** (recipe
   unchanged) → no opam re-installs.

7. **Consumers.** `dag_marshal.ml` doc-comment (universe now = doc-deps `U`; consider
   `version 1→2`). `pages.ml` join by doc-layer hash (C). `sync.ml`/`combine.ml`/
   `odoc_store` read `U` from dag.json by doc-layer hash (D). `status_index.ml`
   per-universe blessed totals (follows from 5).

8. **Tests.** Existing `doc/test/*` pass `universe` explicitly → keep compiling. Add a
   regression test: two solutions reaching the same build hash under two doc-universes
   → assert two distinct compile/link layer hashes and exactly one blessed.

## Verify

`dune build` and `dune build @runtest` after each step. End-to-end: run a profile
containing `odoc.3.2.1`; confirm exactly one blessed doc layer per package, octavius
still works, and the status page shows odoc blessed + rendered.

## Risks

- Step 3 dep-universe resolution: each dep must resolve to *its own* `U` within the
  *same* solution, else mismatched mount sets (overlayfs failures / unresolved xrefs).
- Distinct `(bh,U)` must produce distinct `layer.hash` (Step 2) or the executor
  mis-dedups.
- Step 5 omission double-counts blessed totals.
- Three universe ids (prep / dag.json / served path) must agree end-to-end (D).
