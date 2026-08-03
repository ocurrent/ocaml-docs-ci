# layer — Generic on-disk layer cache primitives

A domain-agnostic abstraction for input-addressed build layers stored
on disk. Each layer is named by a hash of its *build inputs* (base
image, package, dependency hashes) — a Nix-style derivation key, not a
digest of the produced `fs/` bytes. Knows about layer directories,
generic JSON metadata, file
scanning, hardlink merging, and overlay-mount planning. Knows nothing
about opam, packages, doc generation, or any specific build pipeline.

For opam-flavoured layers see the sister library
[`day11_opam_layer`](../opam_layer/), which adds the
package-specific sidecars and helpers.

## External dependencies

- `day11_sys` — sudo wrappers, subprocess execution
- `eio`, `eio.unix` — filesystem and process effects (the API is
  Eio-threaded: most functions take an `Eio_unix.Stdenv.base`)
- `bos`, `fpath`, `rresult`, `unix` — filesystem helpers
- `yojson` + `ppx_deriving_yojson` — `layer.json` serialization

Does NOT depend on `opam-format`, the solver, the doc pipeline, or
the container orchestration.

## On-disk layout

Every layer is a self-contained directory under `{os_dir}/`, named by
the first 12 hex characters of its layer hash (a digest of the build
inputs — see `Hash`):

```
{hash[:12]}/
  layer.json      — generic metadata (see Meta.t)
  layer.log       — captured stdout/stderr from the build
  last_used       — sentinel file; mtime is the LRU timestamp
  fs/             — overlay upper: new/changed files from the build

  build.json      — opam-package-build sidecar (Day11_opam_layer.Build_meta)
  doc.json        — odoc layer sidecar         (Day11_opam_layer.Doc_meta)
  ...             — future domains can add their own sidecars

layer_status.jsonl  — append-only finalisation log (see Layer_status)

packages/{id}/
  {hash[:12]}     — symlink registry, see Symlinks
```

The "kind" of a layer is determined by which sidecar files exist in
its directory, not by a field inside `layer.json`. The generic library
never enumerates the set of possible kinds; each domain library owns
its own sidecar format and the convention for which filename it uses.

**Sidecar files must be valid JSON** (typically a single object at
the top level). This convention is unenforced by the library — no
part of `day11_layer` reads sidecars — but tools like
`day11-layer-cli` rely on it so they can pretty-print sidecar
contents without depending on any domain-specific library.

## Modules

The top-level `Day11_layer` module re-exports the core `Layer` type as
`Day11_layer.t` and exposes every module below.

### `Layer`

The core on-disk layer reference: a hash plus its directory.

```ocaml
type t = { hash : string; dir : Fpath.t }

val of_hash : os_dir:Fpath.t -> string -> t
val hash : t -> string
val dir : t -> Fpath.t
val fs : t -> Fpath.t              (* t.dir / "fs" *)
val meta_path : t -> Fpath.t       (* t.dir / "layer.json" *)
val log_path : t -> Fpath.t        (* t.dir / "layer.log" *)
val pp : Format.formatter -> t -> unit
val exists : Eio_unix.Stdenv.base -> t -> bool
val is_ok : Eio_unix.Stdenv.base -> t -> bool   (* exists && exit_status = 0 *)
```

### `Base`

```ocaml
type t = { hash : string; dir : Fpath.t; image : string }
```

The base image layer at the bottom of every overlay stack. Recursive
build-DAG types live in domain libraries (e.g.
`Day11_opam_layer.Build`).

### `Hash`

Input-addressed hashing primitives — an MD5 (`Digest`) of the build
inputs (NUL-joined), never of the produced `fs/` tree. `layer_hash`
keys a build on its base image, the package name, and all dependency
layer hashes, so identical recipes collide and reuse the cache.

```ocaml
val of_strings : string list -> string
val base_hash : image:string -> string
val layer_hash :
  base_hash:string -> dep_hashes:string list -> pkg:string -> string
```

### `Dir`

```ocaml
val name : string -> string                       (* first 12 hex chars *)
val path : os_dir:Fpath.t -> string -> Fpath.t    (* os_dir / name hash *)
```

The layer-directory naming convention: a layer's directory basename is
the first 12 characters of its layer hash, e.g. `"c9f7404f9f87"`.

### `Meta`

The generic per-layer JSON record stored as `layer.json`.

```ocaml
type timing = (string * float) list

val empty_timing : timing
val timing_field : string -> timing -> float

type t = {
  exit_status : int;
  parent_hashes : string list;
  uid : int;
  gid : int;
  base_hash : string;
  disk_usage : int;
  timing : timing;
  created_at : string;
  failed_dep : string option;
}

val save :
  ?created_at:string ->
  Eio_unix.Stdenv.base -> Fpath.t -> t -> (unit, _) result
val load : Eio_unix.Stdenv.base -> Fpath.t -> (t, _) result
```

Domain-specific information lives in sidecar JSON files in the same
directory, owned by the relevant domain library.

### `Last_used`

```ocaml
val touch : Eio_unix.Stdenv.base -> Fpath.t -> unit
val get : Eio_unix.Stdenv.base -> Fpath.t -> float option
```

Per-layer LRU sentinel. Cheap enough to call from every dep-lookup
path. Split off from `Meta` so a touch doesn't rewrite JSON.

### `Stack`

Layer combining for overlayfs assembly.

```ocaml
val merge :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base ->
  layer_dirs:Fpath.t list -> target:Fpath.t -> (unit, _) result
val merge_no_sudo :
  Eio_unix.Stdenv.base ->
  layer_dirs:Fpath.t list -> target:Fpath.t -> (unit, _) result
val plan_lowerdir :
  available:int ->
  merged_overhead:int ->
  entry_cost:(Fpath.t -> int) ->
  Fpath.t list ->
  Fpath.t list * Fpath.t list
```

`merge` cp-hardlinks the `fs/` of every layer into a single target
directory using `sudo cp --archive --link` (sudo because layer `fs/`
trees are typically root-owned). `merge_no_sudo` is the same without
sudo, for user-owned layers from the native backend. `plan_lowerdir`
decides which layers to keep as separate overlayfs lowerdirs and
which to merge, given a byte budget for the mount-options string.

### `Symlinks`

```ocaml
val ensure :
  Eio_unix.Stdenv.base ->
  packages_dir:Fpath.t -> id:string -> layer_name:string ->
  (unit, _) result
```

A small per-id registry: creates `packages_dir/id/layer_name` pointing
at `../../layer_name`. The `id` is opaque to this library — opam
callers pass `name.version` strings, but anything goes. Idempotent:
an existing symlink at the same path is replaced.

### `Scan`

Directory enumeration: `list_layers` returns `(name, path)` for every
layer dir; `list_package_symlinks` returns `(symlink_name, target)`
pairs for one id (with an optional exclude list). Generic; caller
filters and interprets.

### `Layer_status`

Durable per-os-dir log of layer finalisation outcomes, stored at
`{os_dir}/layer_status.jsonl` (one JSON object per line, last entry
per hash wins).

```ocaml
type entry = { hash : string; exit_status : int; ts : string }

val path : Fpath.t -> Fpath.t
val append : os_dir:Fpath.t -> hash:string -> exit_status:int -> unit
val load : os_dir:Fpath.t -> (string, entry) Hashtbl.t
```

`append` is an atomic `O_APPEND` write, serialised within the process
by a mutex. `load` returns `hash → latest entry`, bootstrapping the
file by walking on-disk layers on first call.

### `Relocations`

Scans a layer's `fs/` tree for non-relocatable absolute paths (e.g.
build-time `/tmp/day11_native_.../_opam/...` references) that would
stop the layer being portable between backends or machines.

```ocaml
val scan :
  Eio_unix.Stdenv.base ->
  layer_fs:Fpath.t -> forbidden:string list ->
  (string * string list) list
val save :
  Eio_unix.Stdenv.base ->
  Fpath.t -> (string * string list) list -> (unit, _) result
val load :
  Eio_unix.Stdenv.base -> Fpath.t -> (string * string list) list option
```

### `Snapshot`

Lightweight before/after filesystem comparison for the native build
backend: snapshot a prefix `(size, mtime, inode)` per file, run the
build, then `diff` to get the list of files produced.

```ocaml
type t

val take : Eio_unix.Stdenv.base -> Fpath.t -> t
val diff : Eio_unix.Stdenv.base -> before:t -> Fpath.t -> string list
val is_empty : t -> bool
val size : t -> int
```

### `Import`

Bootstrap a base layer by exporting a Docker image.

```ocaml
val from_docker :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base ->
  image:string -> layer_dir:Fpath.t -> (unit, _) result
```

`docker create` → `docker export` → extract into `layer_dir/fs/`,
removing the temporary container afterwards. Pulls the image if it
isn't already present locally.

## Error-handling convention

| Operation kind | Returns |
|---|---|
| Read, maybe-exists lookup | `_ option` |
| Read, must-exist or parse | `(_, [> Rresult.R.msg]) result` |
| Write / create / mutate | `(_, [> Rresult.R.msg]) result` |
| Best-effort bookkeeping (LRU touch, status append) | `unit`, silent |

## Testing

Unit tests in `test/` cover every module without requiring root or a
container runtime. They run on any platform where OCaml builds.
Integration tests that actually mount overlayfs live in
`day11/container/test/` and are gated on `DAY11_INTEGRATION=true`.
