# doc — Documentation generation

Everything about odoc: dependency analysis, prep structure creation,
tool layer management, doc combining, and doc distribution.

## External dependencies

- `exec` (filesystem ops, sudo for hardlinks)
- `layer` (layer info, opam file lookup for x-extra-doc-deps)
- `container` (runs odoc_driver_voodoo in containers)
- `opam-format` (opam file parsing for dep analysis)

## Modules

### `Odoc_gen` — doc orchestration

Prepares the directory structure for `odoc_driver_voodoo` and generates
the shell commands to run it.

```ocaml
type doc_result =
  | Doc_success of { html_path : string; blessed : bool }
  | Doc_skipped
  | Doc_failure of string

val doc_result_to_yojson : doc_result -> Yojson.Safe.t
val compute_universe_hash : string list -> string

(** Dependency analysis *)
val analyze_doc_deps :
  OpamFile.OPAM.t ->
  OpamPackage.Name.Set.t * OpamPackage.Name.Set.t * bool
(** Returns (compile_deps, link_deps, needs_separate_phases). *)

val get_post_deps : OpamFile.OPAM.t -> OpamPackage.Name.Set.t
(** Post-only deps: link deps minus compile deps. *)

val get_extra_doc_deps : OpamFile.OPAM.t -> OpamPackage.Name.Set.t
(** Parse x-extra-doc-deps field from opam file. *)

(** Prep structure: copies .cmti/.cmt/.ml/.mli from build layer into
    the directory layout expected by odoc_driver_voodoo. *)
val create_prep_structure :
  source_layer_dir:Fpath.t -> dest_layer_dir:Fpath.t ->
  universe:string -> pkg:OpamPackage.t ->
  installed_libs:string list -> installed_docs:string list ->
  Fpath.t

(** Generate shell command for odoc_driver_voodoo. *)
val odoc_driver_voodoo_command :
  pkg:OpamPackage.t -> universe:string -> blessed:bool ->
  actions:string -> odoc_bin:string -> odoc_md_bin:string ->
  string
```

### `Doc_tools` — doc tool layer management

Two layers:
1. **Driver layer** (shared): `odoc_driver_voodoo`, `sherlodoc`, `odoc-md`
   — built once with OCaml 5.x
2. **Odoc layer** (per OCaml version): `odoc` — must match target compiler
   since `.cmt`/`.cmti` formats are version-specific

```ocaml
(** Driver layer *)
val driver_layer_hash : config:Config.t -> compiler_layers:string list -> string
val driver_layer_name : config:Config.t -> compiler_layers:string list -> string
val driver_layer_path : config:Config.t -> compiler_layers:string list -> Fpath.t
val driver_build_script : config:Config.t -> needs_compiler:bool -> string
val driver_exists : config:Config.t -> compiler_layers:string list -> bool
val has_odoc_driver_voodoo : config:Config.t -> compiler_layers:string list -> bool
val sherlodoc_js_path : config:Config.t -> compiler_layers:string list -> Fpath.t

(** Odoc layer (per OCaml version) *)
val odoc_layer_hash : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> string
val odoc_layer_name : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> string
val odoc_build_script : config:Config.t -> ocaml_version:OpamPackage.t -> needs_compiler:bool -> string
val odoc_exists : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> bool
val has_odoc : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> bool

(** Local repo bind-mount for doc tools, if any *)
val local_repo_mount : config:Config.t -> (string * string) option
```

### `Combine_docs` — local doc aggregation via overlayfs

Mounts all successful doc layers as an overlay filesystem for local
viewing.

```ocaml
type doc_layer = { ... }

val scan_cache : Fpath.t -> doc_layer list
val mount_overlay : ... -> Fpath.t -> (unit, [> Rresult.R.msg]) result
val unmount : Fpath.t -> (unit, [> Rresult.R.msg]) result
val copy_support_files : ... -> (unit, [> Rresult.R.msg]) result
```

### `Sync_docs` — doc distribution via rsync

```ocaml
type doc_entry = { html_path : Fpath.t; blessed : bool; ... }

val scan_cache : Fpath.t -> doc_entry list
val sync :
  ?blessed_only:bool -> ?package_filter:(string -> bool) ->
  ... -> (unit, [> Rresult.R.msg]) result
```

### `Deferred_link` — deferred doc linking

For packages with post deps or x-extra-doc-deps: runs the link+HTML
phase after all packages are built.

```ocaml
val run_global :
  ?doc_layers:(string * string) list ->
  config:Config.t -> (unit, [> Rresult.R.msg]) result
(** Scan for packages needing re-linking and run Doc_link_only phase.
    When [doc_layers] is provided (fork path), uses it directly instead
    of scanning the filesystem. *)
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `odoc_gen.ml` | `Odoc_gen` module (234 lines) |
| `doc_tools.ml` | `Doc_tools` module (179 lines) |
| `combine_docs.ml` | `Combine_docs` module (273 lines) |
| `sync_docs.ml` | `Sync_docs` module (182 lines) |
| `main.ml` | `run_global_deferred_doc_link` → `Deferred_link.run_global` |

## Testing

### Unit tests

- **`Odoc_gen.analyze_doc_deps`** — parse a mock opam file with known
  build/link/test deps. Verify `(compile_deps, link_deps,
  needs_separate_phases)` are correct.
- **`Odoc_gen.get_post_deps`** — opam file with link deps not in
  compile deps returns them.
- **`Odoc_gen.get_extra_doc_deps`** — opam file with
  `x-extra-doc-deps` field returns parsed names.
- **`Odoc_gen.compute_universe_hash`** — deterministic: same dep list
  → same hash. Different list → different hash.
- **`Odoc_gen.create_prep_structure`** — set up a mock build layer
  with `.cmti`/`.cmt` files, verify they're copied into the expected
  odoc directory layout.
- **`Odoc_gen.odoc_driver_voodoo_command`** — verify the command
  string contains expected flags and paths.
- **`Doc_tools`** — `driver_layer_hash` / `odoc_layer_hash` are
  deterministic. `driver_exists` / `odoc_exists` return false for
  empty cache, true after creating the expected layer dir.

### Failure mode tests

- **`Odoc_gen.create_prep_structure` — missing source files:** build
  layer with no `.cmti`/`.cmt` files (e.g. a package that installs
  only binaries). Verify the prep structure is created empty rather
  than erroring.
- **`Odoc_gen.analyze_doc_deps` — no deps:** opam file with empty
  depends → returns `(empty, empty, false)`.
- **`Doc_tools` — tool layer build fails:** the doc tool container
  build returns non-zero. Verify the error propagates and no
  half-built tool layer is left behind.
- **`Sync_docs` — rsync unreachable remote:** `sync` with a
  nonexistent remote → `Error`, not a hang.
- **`Combine_docs` — no doc layers:** `scan_cache` on an empty cache
  dir → returns `[]`. `mount_overlay` with empty layer list →
  `Error` (nothing to mount).

### Fault injection

Doc generation calls into `container` for odoc_driver_voodoo and
into `exec` for filesystem operations. Both can be injected.

- **Via `Fist_container`:** configure `generate_docs` to return
  `None` (failure) for specific packages.
  - Verify `Doc_layer` (in `build`) records `Doc_failure` in
    layer.json.
  - Verify `Deferred_link.run_global` skips packages whose initial
    doc phase failed.
- **Via injectable `Run`:** inject a non-zero exit from
  `odoc_driver_voodoo` → verify the doc layer is marked failed but
  the build layer is unaffected.
- **Missing tool layer injection:** remove or don't create the
  driver layer on disk. Verify `Doc_tools.has_odoc_driver_voodoo`
  returns `false` and doc generation skips cleanly (not an
  exception on a missing path).
- **Malformed prep structure:** inject extra/missing files in the
  prep dir created by `create_prep_structure`. Verify
  `odoc_driver_voodoo` reports a failure rather than producing
  corrupt HTML.

### Integration tests (needs containers)

- **`Combine_docs.scan_cache`** — create mock doc layers, verify scan
  finds them. `mount_overlay` + `unmount` round-trip (needs
  Linux/sudo).
- **`Combine_docs` — unmount after crash:** `mount_overlay`, then
  don't call `unmount` (simulating crash). On next run, verify
  `mount_overlay` detects the stale mount and handles it.
- **`Sync_docs`** — create mock doc entries, verify `scan_cache`
  finds them. `sync` with a local rsync target works.
- **`Deferred_link.run_global`** — build a package with post deps,
  verify deferred linking runs.
