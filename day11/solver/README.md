# solver — Dependency resolution

Everything about solving package constraints. The only library that needs
the 0install solver and git libraries.

## External dependencies

- `exec` (process execution for git commands in `Local_repo`)
- `layer` (solution JSON persistence)
- `opam-0install` (the SAT-based dependency solver)
- `opam-format` (opam file parsing, formula evaluation)
- `git-unix` (git repository introspection for incremental solving)

## Modules

### `Context` — solver context

Reads packages from a `Git_packages.t` index (built from a git
opam-repository). All opam repos are in git, so there's no need
for a separate filesystem-backed context.

Supports `prefer_oldest` (for reproducible solves), doc/post dependency
filtering, user constraints, pinned packages, and tracking which
packages were examined (for incremental reuse).

### `Git_utils` — git repository access

```ocaml
val get_git_repo_store_and_hash : string -> Git.Store.t * Git.Hash.t
val resolve_commit_in_store : Git.Store.t -> string option -> Git.Hash.t
```

### `Git_packages` — package index from git

```ocaml
type t  (** Lazy package index keyed by name *)

val of_commit : Git.Store.t -> Git.Hash.t -> t
val get_versions : t -> OpamPackage.Name.t -> OpamPackage.t list
val get_package : t -> OpamPackage.t -> OpamFile.OPAM.t option
val diff_packages : store:Git.Store.t -> Git.Hash.t -> Git.Hash.t -> OpamPackage.Name.t list
(** Compute which package names changed between two commits. *)
```

### `Local_repo` — local package discovery

For `--local-repo` support: discover `.opam` files in a directory,
compute cache hashes (from git state or file contents), validate repos.

```ocaml
val discover_packages : string -> string list
val repo_hash : string -> string
val find_for_packages : local_repos:string list -> string list -> (string * string list) option
val validate : string list -> (unit, string) result
```

### `Solve` — the solver entry point

```ocaml
val solve :
  config:Config.t -> OpamPackage.t ->
  (OpamPackage.Set.t OpamPackage.Map.t * OpamPackage.Name.Set.t,
   string * OpamPackage.Name.Set.t) result
(** [solve config target] returns [Ok (solution, examined)] or
    [Error (message, examined)]. The [examined] set enables incremental
    solver reuse. *)
```

### `Graph` — dependency graph operations

```ocaml
val topological_sort :
  OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.t list
(** Kahn's algorithm. Returns packages in build order. *)

val pkg_deps :
  OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.t list ->
  OpamPackage.Set.t OpamPackage.Map.t
(** Transitive dependency closure for each package. *)

val extract_dag :
  OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.t ->
  OpamPackage.Set.t OpamPackage.Map.t
(** Extract sub-DAG rooted at a given package. *)

val extract_ocaml_version :
  OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.t option
(** Find ocaml-base-compiler or ocaml-variants in solution. *)
```

### `Opam_env` — opam variable environment

```ocaml
val std_env :
  ?ocaml_native:bool -> ?opam_version:string ->
  arch:string -> os:string -> os_distribution:string ->
  os_family:string -> os_version:string ->
  ?ocaml_version:OpamPackage.t -> unit ->
  string -> OpamVariable.variable_contents option
```

### `Opamh` — opam switch state helpers

```ocaml
val dump_state : Fpath.t -> Fpath.t -> unit
(** [dump_state packages_dir state_file] writes switch-state from
    packages directory. Used when assembling container overlays. *)
```

### `Dot_solution` — graphviz output

```ocaml
val to_string : OpamPackage.Set.t OpamPackage.Map.t -> string
val save : Fpath.t -> OpamPackage.Set.t OpamPackage.Map.t -> unit
```

## Source in day10

| day10 file | What moves here |
|------------|----------------|
| `dir_context.ml` | `Dir_context` module |
| `git_context.ml` | `Git_context` module (if it exists) |
| `git_utils.ml` | `Git_utils` module |
| `git_packages.ml` | `Git_packages` module |
| `local_repo.ml` | `Local_repo` module |
| `opamh.ml` | `Opamh` module |
| `dot_solution.ml` | `Dot_solution` module |
| `main.ml` | `solve`, `topological_sort`, `pkg_deps`, `extract_dag`, `extract_ocaml_version`, `opam_env`/`std_env` |
| `util.ml` | `std_env`, `solution_*` (JSON persistence moves to `layer`) |

## Testing

### Unit tests (needs opam-format, opam-0install)

- **`Graph.topological_sort`** — small DAG (A→B→C, A→C): verify
  order is [C, B, A]. Test single-node graph. Test cycle detection
  if applicable.
- **`Graph.pkg_deps`** — verify transitive closure: if A→B→C, then
  `pkg_deps A` includes B and C.
- **`Graph.extract_dag`** — extract sub-DAG for a leaf vs a root,
  verify correct nodes.
- **`Graph.extract_ocaml_version`** — solution with
  `ocaml-base-compiler.5.1.0` returns it. Solution without returns
  `None`.
- **`Opam_env.std_env`** — verify known variables (`os`, `arch`,
  `os-distribution`) return expected values. Verify `ocaml-native`
  flag works.
- **`Dot_solution`** — `to_string` on a small solution produces valid
  DOT syntax.

### Integration tests (needs opam repos on disk)

- **`Dir_context`** — create a minimal opam repo (2-3 packages with
  deps), create context, verify `candidates` returns expected
  versions. Test `prefer_oldest` flips version order. Test
  `filter_deps` respects doc/post flags.
- **`Solve.solve`** — solve for a package in a minimal repo, verify
  the solution contains expected packages in correct dependency order.
- **`Local_repo`** — create a temp dir with `.opam` files, verify
  `discover_packages` finds them. Verify `repo_hash` is
  deterministic.

### Git-related tests (needs git)

- **`Git_packages`** — init a temp git repo with opam-repo structure,
  commit, verify `of_commit` indexes it correctly. Test
  `diff_packages` between two commits.

### Failure mode tests

- **`Solve.solve` — unsatisfiable:** solve for a package with
  conflicting constraints (e.g. requires two incompatible versions
  of the same dep). Verify `Error (message, examined)` with a useful
  diagnostic, not a solver exception.
- **`Dir_context` — corrupt opam file:** place a malformed opam file
  in the repo. Verify `candidates` skips it (returns it as
  unavailable) rather than crashing the entire solve.
- **`Dir_context` — empty repository:** create context with an empty
  `packages/` dir. Verify `candidates` returns `[]` for any name.
- **`Git_utils` — bad ref:** `resolve_commit_in_store` with a
  nonexistent commit SHA → meaningful `Error`, not an uncaught git
  library exception.
- **`Git_packages` — missing package in commit:** `get_package` for a
  package not in the commit → `None`.
- **`Local_repo` — no opam files:** `discover_packages` on a dir with
  no `.opam` files → returns `[]`.
- **`Local_repo` — not a git repo:** `repo_hash` on a non-git
  directory → falls back to file content hashing (or `Error`).
