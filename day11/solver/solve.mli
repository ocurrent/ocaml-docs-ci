(** Dependency solving.

    The main entry point for resolving package dependencies using opam-0install.
    Takes a target package and an opam-repository, returns a
    {!Day11_solution.Solve_result.t} containing both the build dependency graph
    (acyclic, for build ordering) and the doc dependency graph (may have cycles,
    for odoc cross-referencing). *)

val solve :
  packages:Day11_opam.Git_packages.t ->
  env:(string -> OpamVariable.variable_contents option) ->
  ?constraints:OpamFormula.version_constraint OpamTypes.name_map ->
  ?pins:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?prefer_oldest:bool ->
  ?doc:bool ->
  ?extra_targets:OpamPackage.t list ->
  ?pin_target:bool ->
  ?ocaml_version:OpamPackage.t ->
  OpamPackage.t ->
  (Day11_solution.Solve_result.t, string * OpamPackage.Name.Set.t) result
(** [solve ~packages ~env ?pins ?pin_target ?doc ?ocaml_version target] solves
    the dependencies for [target].

    Returns a {!Day11_solution.Solve_result.t} with both [build_deps] (for
    topological build ordering) and [doc_deps] (for odoc cross-referencing,
    including [{post}] deps and per-package [x-extra-doc-deps]).

    The error case includes the [examined] set so incremental solvers can cache
    failures too.

    When [doc] is true (default), [{with-doc}] dependencies and
    [x-extra-doc-deps] are included in the solve and in [doc_deps]. Set
    [~doc:false] for tool builds that don't need doc deps; in that case
    [doc_deps] equals [build_deps].

    When [ocaml_version] is provided, the compiler is pinned to that version.
    Otherwise defaults to [>= 4.08].

    When [pin_target] is [true] (default), an [=] constraint is added on the
    target's exact version — used for {e all-versions} universe modes where each
    (name, version) pair is solved independently. When [false], the target's
    version is treated as a hint only and the solver picks any version that
    fits. Use this for {e latest-version} profile mode: the input may be the
    latest in the repo, but if the rest of the universe forces a different
    version (e.g. an oxcaml [+ox] variant), the solver lands there instead of
    failing. *)
