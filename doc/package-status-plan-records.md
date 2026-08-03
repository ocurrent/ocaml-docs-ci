# Per-package plan records → shared layer status

## Problem

`/profiles/<profile>/p/<pkg>/<ver>` renders "No history entries" for a
package that clearly built and failed (e.g. `eliom-fix` /
`aacplus.0.2.2`, whose build job ran and failed).

Root cause: the per-version page (since `d241f4b`) is driven entirely by
per-profile `history.jsonl`, which is written **at dispatch time, under
the profile that dispatched the layer**. Build layers are
content-addressed and the OCurrent cache is shared across profiles, so a
shared build-dep is dispatched by whichever profile reaches it first
(here `full`); its failure is recorded in `full`'s snapshot, and
`eliom-fix`'s node reuses the cached result without dispatching — so
`eliom-fix` has no history row for it, and the page shows nothing.

Two lifetimes are conflated:

- **Structural** — "this profile's plan contains this pkg.version at
  these node hashes, in this snapshot." Known at *plan time*, for
  *every* package, per profile. Currently only in `dag.json`
  (per-snapshot, 9 MB, and the in-memory plan is latest-only).
- **Outcome** — "did this hash build, and where is its job." A *shared*,
  dispatch-time fact. `exit_status` already lives in the shared
  per-os_dir `layer_status.jsonl`; the `job_id` is currently scraped
  from OCurrent's own sqlite cache.

## Design

Separate the two and join them in the page.

### 1. Plan-time, per-package plan record (this change)

At plan time (`generate.ml`, after solving, alongside `dag.json`), write
for **every** package.version in the profile's plan:

    snapshots/<profile>/<snap>/packages/<pkg>.<ver>/plan.json
      [ {"hash","kind","universe","blessed"} , ... ]   # one per node

- Written for all planned packages, not just dispatched ones → a
  build-failed or another-profile-built package still gets a record.
- Per-snapshot (like `history.jsonl`), so the page reads them across
  snapshots and preserves the multi-snapshot view — cheaply, since each
  file is tiny (no 9 MB `dag.json` parse).
- Makes `packages/<pkg>.<ver>/` always exist for a planned package,
  which structurally removes the "dir absent → No history" case.

### 2. Shared outcome lookup (reuse existing)

- `exit_status`: `layer_status.jsonl` (shared per-os_dir, already loaded
  by the web via `load_layer_status_cached`; derived by scanning each
  layer's `layer.json`).
- `job_id`: for now, the existing `job_ids_for_hashes` sqlite lookup
  against OCurrent's cache (kept as-is; the Rebuild button stays
  OCurrent's, auth intact — we only need the id as a link target).

### 3. Page (`pages.ml` `package_version`)

When per-profile `history.jsonl` has entries, render as today (rich
detail: ts/run/error). When it does **not**, fall back: read
`plan.json` across snapshots, and for each node synthesise a row —
status from `layer_status` (`exit_status` 0 = ok, ≠0 = failed, absent =
not built/pending), hash → `/job/<job_id>` link via the sqlite lookup.
This shows cross-profile / build-failed packages (aacplus) instead of
"No history entries."

## Follow-up (separate change): retire the sqlite scrape

`job_id` cannot go in `layer_status` (it's scan-derived from
`layer.json`, and `job_id` must not live in layer metadata). Give it its
own shared per-os_dir index — `<os_dir>/job_index.jsonl`
(`hash → job_id`), appended from `day11_prep.Op.build` (which holds
`Current.Job.id job`). Then the page resolves the job link from our own
index and the OCurrent-cache scrape (`job_ids_for_hashes`) and its
standing TODO can be deleted. The Rebuild action + auth remain
OCurrent's — we only store the id to build the link.
