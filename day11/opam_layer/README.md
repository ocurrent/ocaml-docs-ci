# opam_layer — Opam-flavoured layer types and sidecars

Domain-specific extensions on top of [`day11_layer`](../layer/) for
opam package builds. This is the only place in day11 that knows what
an "opam package layer" actually is — the generic layer library
underneath knows nothing about opam.

For odoc documentation layers, see the sister library
[`day11_doc_layer`](../doc_layer/), which is independent of this one
and depends only on `day11_layer`.

## What it adds to the generic library

- **Recursive in-memory build types** (`Build.t`, `Tool.t`) used by
  the planner and executor
- **`build.json` sidecar** (`Build_meta.t`) for opam package build
  layers
- **Opam switch helpers**: `Installed_files`, `Opam_repo`, `Opamh`

## External dependencies

- `day11_layer` — generic layer cache
- `day11_sys` — sudo wrappers
- `day11_graph` — `Universe.t`
- `opam-format` — `OpamPackage.t` and switch state
- `bos`, `fpath`, `rresult`, `yojson` + `ppx_deriving_yojson`

## Modules

### `Build` — recursive in-memory build node

```ocaml
type t = {
  hash : string;
  pkg : OpamPackage.t;
  deps : t list;
  universe : Day11_graph.Universe.t;
}
val dir_name : t -> string
val dir : os_dir:Fpath.t -> t -> Fpath.t
```

The DAG node used throughout the planner, hash cache, and executor.
[`day11_opam_build`](../build/) takes these as input.

### `Tool`

```ocaml
type t = { hash : string; dir : Fpath.t; builds : Build.t list }
```

An aggregate of one or more `Build.t` layers (e.g. odoc plus its
deps), used as a fixed input to other build containers — the doc
pipeline takes a `Tool.t` for each compiler that needs odoc binaries
mounted.

### `Build_meta` — `build.json` sidecar

Marks a layer as the result of an opam package build.

```ocaml
type t = {
  package : string;
  deps : string list;          (* parallel to layer.json's parent_hashes *)
  installed_libs : string list;
  installed_docs : string list;
  patches : string list;
}

val filename : string             (* "build.json" *)
val save : Fpath.t -> t -> (unit, _) result
val load : Fpath.t -> (t, _) result
val exists : Fpath.t -> bool
val load_tree : os_dir:Fpath.t -> string -> (Build.t, _) result
```

`load_tree` reconstructs a `Build.t` tree from the cache by recursively
walking parent_hashes in `layer.json` plus the package field in
`build.json`.

### `Installed_files` — opam switch scanner

```ocaml
val scan_libs : layer_dir:Fpath.t -> string list
val scan_docs : layer_dir:Fpath.t -> string list
```

Walks `layer_dir/fs/home/opam/.opam/default/{lib,doc}/` for
`.cmi`/`.cmxa`/`META`/`.mld`/etc. Used by `Build_layer` after a
container exits to populate `Build_meta.installed_libs/installed_docs`.

### `Opam_repo` — opam-repository assembly

```ocaml
val create : Fpath.t -> (Fpath.t, _) result
val populate :
  opam_repo:Fpath.t -> opam_repositories:Fpath.t list ->
  OpamPackage.t list -> (unit, _) result
```

Builds a minimal `opam-repository/` tree containing just the opam
files needed for a package and its deps.

### `Opamh` — opam switch state

```ocaml
val compiler_packages : OpamPackage.Name.t list
val dump_state : Fpath.t list -> Fpath.t -> (unit, _) result
```

Identifies compiler packages and writes a `switch-state` file
describing what's installed inside a container.

## Why split

The generic [`day11_layer`](../layer/) library can be linked without
pulling in `opam-format` or any opam-specific concepts. This makes it
suitable for layer-cache use cases unrelated to opam. The opam
specifics live here. The odoc-specific format lives in
[`day11_doc_layer`](../doc_layer/).
