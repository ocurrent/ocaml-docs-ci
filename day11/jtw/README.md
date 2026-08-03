# jtw — JavaScript/REPL artifact generation

Generates JavaScript artifacts for in-browser OCaml REPLs using
`js_of_ocaml` and `js_top_worker`.

## External dependencies

- `exec` (filesystem ops)
- `layer` (layer info)
- `container` (runs js_of_ocaml in containers)
- `opam-format` (`OpamPackage`)
- `str` (regex for CMI parsing)

## Modules

### `Jtw_gen` — JTW layer logic

```ocaml
val compute_jtw_layer_hash : build_hash:string -> jtw_tools_hash:string -> string

(** Generate dynamic_cmis.json for a directory of .cmi files.
    Partitions into hidden (__) and toplevel modules. *)
val generate_dynamic_cmis_json : dcs_url:string -> string list -> string

(** Generate findlib META index for a set of installed libs. *)
val generate_findlib_index : ... -> string

(** Content hashing for cache invalidation. *)
val compute_content_hash : Fpath.t -> string
val compute_compiler_content_hash : Fpath.t -> string

(** Findlib package names from installed lib files. *)
val findlib_names_of_installed_libs : string list -> string list

(** Container scripts for JTW generation. *)
val jtw_container_script : ... -> string
val jtw_worker_container_script : ... -> string

(** Assemble final JTW output directory with content-hashed paths. *)
val assemble_jtw_output :
  config:Config.t -> jtw_output:string ->
  solutions:... -> blessed_maps:... -> unit

(** Save JTW layer metadata. *)
val save_jtw_layer_info : ... -> (unit, [> Rresult.R.msg]) result
```

### `Jtw_tools` — JTW tool layer management

Per OCaml version: installs `js_of_ocaml` and `js_top_worker`, builds
`worker.js`, extracts stdlib CMIs.

```ocaml
val layer_hash : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> string
val layer_name : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> string
val layer_path : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> Fpath.t
val build_script : config:Config.t -> ocaml_version:OpamPackage.t -> needs_compiler:bool -> string
val exists : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> bool
val has_jsoo : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> bool
val has_worker_js : config:Config.t -> ocaml_version:OpamPackage.t -> compiler_layers:string list -> bool
val local_repo_mount : config:Config.t -> (string * string) option
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `jtw_gen.ml` | `Jtw_gen` module (431 lines) |
| `jtw_tools.ml` | `Jtw_tools` module (85 lines) |

## Testing

### Unit tests

- **`Jtw_gen.compute_jtw_layer_hash`** — deterministic: same inputs →
  same hash.
- **`Jtw_gen.generate_dynamic_cmis_json`** — provide a list of CMI
  names, verify output JSON structure. Verify `__`-prefixed modules
  go to hidden list.
- **`Jtw_gen.generate_findlib_index`** — verify META-like output for
  known libs.
- **`Jtw_gen.compute_content_hash`** — same file → same hash.
  Different file → different hash.
- **`Jtw_gen.findlib_names_of_installed_libs`** — map known lib paths
  to findlib package names.
- **`Jtw_tools`** — `layer_hash` is deterministic.
  `exists` / `has_jsoo` / `has_worker_js` return false on empty cache.

### Failure mode tests

- **`Jtw_gen.generate_dynamic_cmis_json` — empty list:** provide an
  empty CMI list → produces valid JSON with empty arrays (not an
  error).
- **`Jtw_gen.compute_content_hash` — missing file:** hash a
  nonexistent path → `Error`, not an uncaught exception.
- **`Jtw_tools` — tool layer build fails:** container build for
  js_of_ocaml returns non-zero. Verify `Error` and no half-built
  tool layer left on disk.
- **`Jtw_gen.assemble_jtw_output` — no successful JTW layers:**
  assemble with all packages failed → produces empty output dir
  rather than crashing.

### Fault injection

- **Via `Fist_container`:** configure `generate_jtw` to return
  `None` for specific packages. Verify `Jtw_layer` (in `build`)
  records the failure and `assemble_jtw_output` skips the package
  cleanly.
- **Via injectable `Run`:** inject a non-zero exit from
  `js_of_ocaml` → verify the JTW layer is not created and no
  partial `worker.js` is left on disk.
- **Missing tool layer:** don't create the jtw-tools layer on disk.
  Verify `Jtw_tools.has_jsoo` returns `false` and JTW generation
  is skipped rather than crashing on a missing path.

### Integration tests (needs js_of_ocaml in a container)

- **`Jtw_gen.jtw_container_script`** — verify the generated shell
  script is syntactically valid (`bash -n`).
- End-to-end: build a simple package, generate JTW layer, verify
  `worker.js` exists.
