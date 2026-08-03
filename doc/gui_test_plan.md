# GUI test plan

Coverage plan for the dashboard pages in `src/web/` (served at
`http://<host>:8080/profiles/...`). Organized in four layers: (0)
setup, (1) user journeys from a human operator's perspective, (2)
feature-by-feature systematic coverage, (3) historical-sweep
integration, (4) edge cases and robustness. The historical sweep
feeds directly into several tests because it creates the diverse
snapshot data needed to exercise diff, history, and pagination
meaningfully.

---

## 0. Setup

**Prereqs**
- Pipeline running at `http://localhost:8080` with `--profiles
  ocaml-small,oxcaml-small` and both `--remote` mirrors configured.
- `day11 batch` has run at least once per profile so
  `html-<profile>/` and `.day11/snapshots/<profile>/` are populated.

**Seed data for interesting tests** — run the historical sweep:
```bash
/tmp/ocaml-small-hist.sh     # 10 snapshots, commits from
                             # 2025-07-16 → 2026-04-21
```
After this, `ocaml-small` has a realistic timeline: odoc 3.1.0
release, nine months of repo evolution, today's HEAD. This is the
dataset the user-journey tests assume.

**Smoke test** — before anything else:
```bash
curl -sf http://localhost:8080/profiles > /dev/null && echo OK
```

---

## 1. User-journey tests (what a human is actually trying to do)

Each journey is a workflow with an endpoint. Mark "passed" only if
the GUI gets you there without needing to fall back to
`ls ~/.day11` or `grep`.

### J1. "Which packages failed in last night's run?"
1. `/profiles` → pick `ocaml-small`.
2. Profile dashboard shows latest snapshot SHA and date.
3. Click into latest snapshot. **Expect:** status-totals table
   shows failed count > 0 (or 0 and you're done).
4. Package list below is navigable; failed packages should be
   distinguishable at a glance.

   **Gap to note if present:** currently `snapshot_detail` shows a
   flat list and per-status totals — there is no per-package
   status column. Record whether you can actually identify the
   failures from this page alone, or whether you have to click
   each package.

### J2. "Why did `astring` fail?"
1. From snapshot detail → click `astring.X.Y.Z` package link.
2. Lands on `/profiles/ocaml-small/p/astring/X.Y.Z` with a history
   table.
3. **Expect:** `Error` column populated for the failing row;
   status span colored.
4. Record: can you get to the actual build log from here?
   (Probably not directly — the OCurrent job log lives under
   `/job/...`. Note whether the "Hash" column could/should link
   to it.)

### J3. "When did this regression start?"
1. Same package-version page as J2. History should span the 10
   historical snapshots.
2. **Expect:** status flips from `success` → `failed` at a
   specific row. That row's `Run` timestamp tells you when.
3. Copy the two adjacent snapshot SHAs, then navigate to
   `/profiles/ocaml-small/snapshots/<older>/diff/<newer>` — the
   diff should reveal the repo change that caused it.

### J4. "What changed between today and yesterday?"
1. `/profiles/ocaml-small/snapshots` → grab two adjacent SHAs.
2. Manually construct
   `/profiles/ocaml-small/snapshots/<old>/diff/<new>`.
   **UX gap to record:** there is currently no "Diff against
   previous" button on the snapshot detail page — the URL must be
   hand-built. Note this for follow-up.
3. Diff table shows only changed rows (added / removed / version
   bump / status change).

### J5. "Does `fmt.0.11.0` still render docs correctly on oxcaml?"
1. `/profiles/oxcaml-small/snapshots/<latest>` → click
   `fmt.0.11.0`.
2. "Open rendered docs" →
   `/profiles/oxcaml-small/docs/p/fmt/0.11.0/doc/index.html`.
3. Click into `Fmt` module page.
4. **Expect:** zero `xref-unresolved` spans for `Stdlib`. (As of
   today's state this *fails* for `fmt.0.11.0` — see note at
   end.) Use this as the canary. The shell test
   `test_stdlib_xrefs.sh` gives the machine-checkable version.

### J6. "I just pushed to opam-repository — did the pipeline pick it up?"
1. Note current HEAD of `/home/jjl25/ocaml/opam-repository`.
2. `/profiles/ocaml-small` → latest snapshot → repos table.
3. **Expect:** commit column matches HEAD within one pipeline
   tick (or close).

---

## 2. Feature-by-feature coverage

### F1. `/profiles` (index)
- [ ] Lists every profile with a `.json` in `~/.day11/profiles/`.
- [ ] Does *not* list orphan snapshot dirs (e.g. `smoke`) when
      there's no matching JSON.
- [ ] "Snapshots" count matches
      `ls ~/.day11/snapshots/<name>/ | wc -l`.
- [ ] "Latest" column is a linkable short SHA; clicking lands on
      snapshot detail.
- [ ] Empty profile (0 snapshots on disk) renders `—` for latest
      and `0` for count — no crash.

### F2. `/profiles/<name>` (dashboard)
- [ ] Shows latest snapshot SHA + "All snapshots (N)" link.
- [ ] Profile with 0 snapshots shows "No snapshots yet for this
      profile."
- [ ] Non-existent profile name (`/profiles/bogus`) → clean
      error, not 500.

### F3. `/profiles/<name>/snapshots` (list + pagination)
- [ ] Ordering: newest-first by mtime.
- [ ] Timestamp column formatted `YYYY-MM-DD HH:MM` UTC.
- [ ] **Pagination** — `page_size = 25`. Test specifically:
  - Profile with 0 snapshots: no pager, empty table.
  - Profile with 1 snapshot: no pager, 1 row.
  - Profile with exactly 25: no pager (n_pages = 1).
  - Profile with 26: pager appears, "Page 1 of 2 (26 snapshots)",
    "Next ›" visible.
  - After the historical sweep `ocaml-small` should have ≥ 10
    snapshots. Temporarily cap `page_size` to 5 in source,
    rebuild, verify pager renders with 3 pages.
- [ ] `?page=2` advances page; `?page=9999` clamps to last page;
      `?page=abc` falls back to page 1 (not a 500).
- [ ] `?page=0` or negative: treated as page 1.

### F4. `/profiles/<name>/snapshots/<key>` (detail)
- [ ] Repos table lists every entry from `repos.json`; commit
      SHAs shown as short form.
- [ ] Status totals: "Blessed total" / "Non-blessed total" match
      hand-sum of `status_index.json`.
- [ ] When `status_index.json` is absent (build in progress),
      page shows "Status not yet generated…" without crashing.
- [ ] When `repos.json` is absent, page shows "No repos.json on
      disk." without crashing.
- [ ] Package list: count matches
      `ls <snapshot>/packages/ | wc -l`.
- [ ] Package-list truncation: for a snapshot with > 200
      packages, list shows first 200 + "(N more, truncated)".
      Verify against `oxcaml-small` latest (280 pkgs).
- [ ] Each package link splits `name.version` at the first `.`
      correctly:
  - `astring.0.8.5` → `/p/astring/0.8.5` ✓
  - `base-bigarray.base` → `/p/base-bigarray/base` ✓
  - `base.v0.18~preview.130.91+190` →
    `/p/base/v0.18~preview.130.91+190` — verify end-to-end.
- [ ] Invalid snapshot key (`/snapshots/deadbeef00ff`) → clean
      error.

### F5. `/profiles/<name>/snapshots/<old>/diff/<new>`
- [ ] Same key for both sides: "No differences."
- [ ] Oldest → newest (across sweep): large table, mostly "added"
      rows for packages that didn't exist in the July snapshot.
- [ ] Newest → oldest: same rows labelled "removed".
- [ ] Two adjacent sweep snapshots: small diff; rows categorized
      as `version: X → Y`, `status changed`, `added`, `removed`
      and nothing else.
- [ ] Non-existent key on either side → empty or clean error.
- [ ] Status change without version change: labelled `status
      changed` specifically.

### F6. `/profiles/<name>/p/<pkg>` (versions)
- [ ] Lists every version found across all snapshots, sorted,
      deduped.
- [ ] Click into each version → package-version page.
- [ ] Unknown package name → "No builds of this package in any
      snapshot."

### F7. `/profiles/<name>/p/<pkg>/<ver>` (history)
- [ ] Row count matches
      `cat <snap>/packages/<pkg>.<ver>/history.jsonl | wc -l`
      summed across snapshots.
- [ ] Columns: Time / Run / Status / Category / Hash / Compiler /
      Error all populated for a known failing build.
- [ ] Status column colored via `Templates.status_span`.
- [ ] "Open rendered docs" link resolves to HTTP 200 when the
      package has docs in
      `html_dir/p/<pkg>/<ver>/doc/index.html`.
- [ ] "Open rendered docs" link returns 404 (not 500) when the
      package wasn't doc-built in this profile — verify on a
      base/virtual pkg like `base-bigarray.base`.

### F8. `/profiles/<name>/docs/<path...>` (static serving)
- [ ] `docs/odoc.css` → 200, `Content-Type: text/css`.
- [ ] `docs/p/astring/0.8.5/doc/index.html` → 200,
      `Content-Type: text/html; charset=utf-8`.
- [ ] `docs/fonts/KaTeX_Main-Regular.woff2` → 200,
      `Content-Type: font/woff2`.
- [ ] Missing file → 404 "file not found".
- [ ] **Path traversal defences:**
  - `docs/../../../etc/passwd` → 400 "bad path".
  - `docs/p/../../../etc/passwd` → 400.
  - `docs/%00.html` (URL-encoded null) → 400.
  - Absolute-path probe `docs//etc/passwd` → resolves inside root
    (leading slashes are dropped), so this is a 404, not a
    traversal.
- [ ] Profile with `html_dir: null` → 404 with "profile X has no
      html_dir configured".
- [ ] Non-existent profile → 404 with "no such profile".
- [ ] HEAD request → 400 "Bad method" (framework-level; document
      this, don't treat as bug).
- [ ] Deep path works:
      `docs/p/fmt/0.11.0/doc/fmt/Fmt/Dump/index.html` → 200.
- [ ] Bytes served identical to on-disk: `md5sum` the served body
      vs the file.

### F9. Shared template (`Templates`)
- [ ] `style_block` CSS injected on every page (inspect one
      page's `<head>`).
- [ ] `breadcrumbs` on every non-index page; each crumb is a link
      except the last.
- [ ] `sha_span` consistently shows 12-char truncation with
      monospace styling.
- [ ] `status_span` colors: success / failed / error / skipped
      visibly differ.

---

## 3. Historical-sweep integration tests

After `/tmp/ocaml-small-hist.sh` completes, these should all hold
for `ocaml-small`:

### H1. Snapshot count and ordering
- [ ] `/profiles/ocaml-small/snapshots` lists 10 rows (plus any
      pre-existing).
- [ ] Sorted newest-first; `2026-04-21`-indexed commit at top,
      `2025-07-16` at bottom.
- [ ] Timestamps monotonically decrease.

### H2. Diff across a year
- [ ] Diff `5626e0ce9828` (oldest) → `4375a073acc0` (newest):
      hundreds of `added` / `version:` rows. No `removed` rows
      expected (opam-repository grows).
- [ ] Reverse direction mirrors to `removed`.

### H3. Package timeline
- [ ] Pick a stable pkg like `fmt`: `/profiles/ocaml-small/p/fmt`
      should list multiple versions (0.9.0, 0.10.0, 0.11.0 at
      least).
- [ ] Each version's history page has ≤ 10 rows (one per sweep
      snapshot that carried it).
- [ ] A version that was added mid-sweep (e.g. `fmt.0.11.0`,
      released 2025-09-19) should have history rows *only* from
      that sweep snapshot onward — verify by comparing entry
      timestamps with the commit dates in the sweep script.

### H4. Failure archaeology
- [ ] Locate a package that failed in *some* sweep snapshots but
      not others via the history page. Confirm the `Error`
      column populates; confirm clicking the snapshot link jumps
      to the right snapshot detail.

### H5. Doc deep-link survives old builds
- [ ] For today's HEAD snapshot,
      `/profiles/ocaml-small/docs/p/<pkg>/<ver>/doc/index.html`
      works.
- [ ] Note: `html_dir` is overwritten each run (no historical
      HTML preserved). Document this as a limitation — the doc
      link reflects the *latest render*, not the snapshot's
      render.

---

## 4. Edge cases & robustness

### E1. Empty / missing state
- [ ] Delete `~/.day11/profiles/foo.json` while server runs →
      `/profiles/foo` still served? (cached vs live read — record
      behaviour.)
- [ ] Rename a snapshot dir away mid-session →
      `/profiles/<name>/snapshots` next request reflects removal.
- [ ] Add a new profile JSON → `/profiles` reflects it without
      restart (live read path).

### E2. URL character handling
- [ ] Package name with tilde or plus:
      `base.v0.18~preview.130.91+190` round-trips through
      `/p/base/v0.18~preview.130.91+190`.
- [ ] Query string `?page=abc` → page 1 (no 500).
- [ ] Query string `?page=-1` → page 1.
- [ ] Trailing slashes: `/profiles/` vs `/profiles` — both should
      work (Routes library behavior).
- [ ] Double-slash: `/profiles//oxcaml-small` — document
      behavior.

### E3. Concurrency
- [ ] While a build is actively writing
      `packages/<pkg>/history.jsonl`, hit the package-version
      page → response succeeds (no torn read) or cleanly falls
      back.

### E4. Browser viewing (manual pass)
Open these in Firefox/Chrome and *look* at them:
- [ ] `/profiles` — table legible, links obvious.
- [ ] Snapshot detail with 280 packages — scroll performance OK;
      no horizontal overflow.
- [ ] Rendered `Fmt` doc page inside `/profiles/.../docs/...` —
      CSS loaded, syntax highlighting active, sidebar functional.
- [ ] Breadcrumbs appear on every non-index page, clickable and
      correct.
- [ ] Go `/profiles → oxcaml-small → latest snapshot →
      fmt.0.11.0 → Open rendered docs → Fmt module` without
      using the URL bar.

### E5. Known caveats to verify don't regress
- [ ] Profile dashboard has *no* "Browse rendered docs" link
      (removed because html_dir has no root index.html).
- [ ] `snapshot_detail` package links use
      `/p/<name>/<version>`, not `/p/<name>.<version>`.
- [ ] Stale snapshot dirs without a matching JSON profile do not
      appear in `/profiles`.

### E6. Security sanity
- [ ] `curl 'http://localhost:8080/profiles/oxcaml-small/docs/..%2f..%2f..%2fetc%2fpasswd'`
      → 400 or 404, never the file.
- [ ] `/profiles/oxcaml-small/docs/` (with trailing slash, empty
      tail) → serves `index.html` from html_dir, or 404 if no
      index.html. Never crashes.

---

## 5. Regression tests to automate

These are cheap enough to turn into CI shell scripts:
1. `test_stdlib_xrefs.sh` (already exists) — doc-pipeline canary.
2. **`test_gui_smoke.sh`** (new, proposed): curl every route,
   assert 200 except path-traversal (400) and nonexistent (404).
   Walk from `/profiles` → leaf rendered doc in a scripted
   breadth-first crawl, asserting no 5xx anywhere.
3. **`test_pagination.sh`** (new, proposed): hit
   `?page=1,2,9999,abc,-1,0` and assert all return 200 with sane
   page headers.
4. **`test_static_mime.sh`** (new, proposed): serve each of
   `.html .css .js .svg .woff2 .json` and assert `Content-Type`.

---

## Outstanding observations flagged during planning

- **No per-package status column on snapshot detail** → J1 is
  harder than it should be. Consider adding a status badge next
  to each package link.
- **No "Diff against previous" button** → J4 requires
  hand-constructing URLs.
- **No link from history rows to OCurrent job logs** → J2 forces
  a context switch to `var/job/...`.
- **`fmt.0.11.0` Stdlib xrefs unresolved** — pre-existing
  doc-pipeline ordering bug, tracked separately but surfaced by
  J5.
- **html_dir is a single live mirror** — H5 limitation; if
  historical rendered docs are ever needed, snapshot-scoped
  `html_dir`s would be required.
