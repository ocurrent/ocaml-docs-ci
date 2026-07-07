(** Stateless per-package doc build primitives.

    Each function takes explicit inputs (layer directories, tool
    directories, package metadata) and produces a result. No shared
    mutable state, no DAG knowledge, no hashtable lookups. These are
    the building blocks for both the DAG-based batch executor and
    potential integration with external pipeline systems like OCurrent.

    All functions follow the same pattern:
    - Check preconditions (installed libs, tool availability)
    - Set up container mounts and prep structure
    - Run odoc_driver_voodoo in a container via Build_layer.build
    - Return the resulting layer directory or None on failure *)

type doc_config = {
  driver_tool : Day11_opam_layer.Tool.t;
  odoc_tool : Day11_opam_layer.Tool.t;
  os_dir : Fpath.t;
  blessed : bool;
}
(** Static configuration for a doc build. The tools and blessed status
    are determined before execution and don't change across packages
    within the same compiler universe. *)

val compile :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  config:doc_config ->
  build_layer:Fpath.t ->
  universe:string ->
  build_deps_layers:Fpath.t list ->
  dep_compile_layers:Fpath.t list ->
  hash:string ->
  OpamPackage.t ->
  (Fpath.t, string) result
(** [compile env benv ~config ~build_layer ~universe ~build_deps_layers
    ~dep_compile_layers ~hash pkg] runs the odoc compile phase for [pkg].
    [universe] is the doc-deps universe id ([Universe.of_deps] of the
    transitive doc_deps closure); it namespaces the prep tree so the same
    build layer compiled under different universes yields distinct outputs.
    Reads source [.cmti]/[.ml] files from [build_layer], stacks
    [build_deps_layers] (for [ocamlobjinfo] and other build tools) and
    [dep_compile_layers] (for cross-reference resolution), and produces
    a compile layer at [os_dir/<hash>/].

    {b Dep closure required:} [dep_compile_layers] must be drawn from
    the {b transitive closure of [build_deps]}, never [doc_deps]. The
    compile phase consumes [.cmti] files which only reference packages
    the OCaml compile saw. See {!page-doc_dep_graphs} §2.

    Returns [Ok layer_dir] on success, [Error msg] on failure. *)

val link :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  config:doc_config ->
  build_layer:Fpath.t ->
  universe:string ->
  build_deps_layers:Fpath.t list ->
  compile_layer:Fpath.t ->
  dep_compile_layers:Fpath.t list ->
  html_dir:Fpath.t ->
  hash:string ->
  OpamPackage.t ->
  (unit, string) result
(** [link env benv ~config ~build_layer ~build_deps_layers ~compile_layer
    ~dep_compile_layers ~html_dir ~hash pkg] runs the odoc link phase
    for [pkg]. Reads [.odoc] files from [compile_layer] and
    [dep_compile_layers], writes HTML to [html_dir]. Stacks
    [build_deps_layers] for build-tool access (ocamlobjinfo etc.).

    {b Dep closure required:} [dep_compile_layers] must be drawn from
    the {b transitive closure of [doc_deps]} (which already includes
    [{post}] and [x-extra-doc-deps]). Cross-references in docstrings
    are resolved here, and may target packages that the compile phase
    didn't see. See {!page-doc_dep_graphs} §3.

    Returns [Ok ()] on success. The link layer itself is ephemeral —
    only the HTML output matters. *)

val doc_all :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  config:doc_config ->
  build_layer:Fpath.t ->
  universe:string ->
  build_deps_layers:Fpath.t list ->
  dep_compile_layers:Fpath.t list ->
  html_dir:Fpath.t ->
  hash:string ->
  OpamPackage.t ->
  (Fpath.t, string) result
(** [doc_all env benv ~config ~build_layer ~universe ~dep_compile_layers
    ~html_dir ~hash pkg] runs compile + link + HTML generation in a
    single container invocation. Returns the compile layer directory
    (which contains the [.odoc] output for use by dependents).

    {b Precondition:} only valid for packages where
    [build_deps == doc_deps]
    (see {!Day11_doc.Doc_deps.needs_separate_link}). [dep_compile_layers]
    is drawn from the transitive closure of [build_deps]
    (equivalently, [doc_deps]). See {!page-doc_dep_graphs} §4. *)

val has_documentable_libs :
  Fpath.t -> bool
(** [has_documentable_libs layer_dir] returns true if the build layer
    has installed libraries that can be documented.
    {b Note:} requires the layer to be built. For doc DAG planning,
    prefer {!is_ocaml_package} which uses the solver result and so
    works on a cold cache. *)

val concrete_compiler_names : OpamPackage.Name.t list
(** Names of the real-compiler packages — the ones whose build
    installs [lib/ocaml/stdlib*.cmti]. The actual package per name
    depends on version (see {!is_compiler_pkg}); this list is just
    the {e set of names} for cheap prefilter matching. *)

val is_compiler_pkg : OpamPackage.t -> bool
(** [is_compiler_pkg pkg] returns true when [pkg] is the real
    compiler (the one that installs stdlib) for its release line:
    {ul
      {- [ocaml-compiler] {b ≥} 5.3.0 — modern mainline.}
      {- [ocaml-base-compiler] {b <} 5.3.0 — pre-split mainline.}
      {- [oxcaml-compiler] (any version).}}
    Returns false for wrappers ([ocaml-variants], post-5.3.0
    [ocaml-base-compiler], [ocaml-system]) which are tagged
    [flags: compiler] in opam but have empty installs. *)

val is_ocaml_package :
  Day11_opam_layer.Build.t -> bool
(** [is_ocaml_package node] returns true if [node]'s {e transitive}
    build closure includes the virtual package [ocaml], OR [node] is
    one of the {!concrete_compiler_names} (which {e provides} [ocaml]).
    The second disjunct ensures the compiler's stdlib is documented;
    without it, every package's [Stdlib.*] xref in the rendered
    HTML lands as [xref-unresolved]. The closure (not just direct
    deps) is walked so packages that reach [ocaml] only through a
    dependency — e.g. [dune-rpc]/[dune-rpc-lwt] via [lwt]/[stdune],
    whose own opam files don't list [ocaml] — are still documented.
    [conf-*] system packages, [ocaml-options-*], and [ocaml-config]
    don't reach [ocaml] even transitively and so are still skipped.
    Unlike {!has_documentable_libs}, this works without the layer
    being built — useful for the first run on a cold cache. *)

val check_stdlib_installed :
  build_layer:Fpath.t ->
  OpamPackage.t ->
  (unit, [> Rresult.R.msg ]) result
(** Post-build invariant: a package classified as a compiler by
    {!is_compiler_pkg} is expected to have left
    [lib/ocaml/stdlib.cmti] in its build layer. Return [Ok ()] for
    non-compiler packages, [Ok ()] when the file is present, and
    [Error] otherwise — flagging either a broken upstream build or
    a misclassification in {!is_compiler_pkg}. *)

