(** Global DAG construction across solutions.

    Builds deduplicated DAGs of build, doc compile, and doc link nodes from
    multiple solved targets. *)

val build_dag :
  Hash_cache.t ->
  base_hash:string ->
  (OpamPackage.t * Day11_solution.Deps.t * Day11_solution.Deps.t) list ->
  Day11_opam_layer.Build.t list
(** [build_dag cache ~base_hash solutions] builds a deduplicated DAG of build
    nodes across all solutions. Each solution is
    [(target, build_deps, doc_deps)]. The build LAYER hash is computed from each
    package's transitive build-deps closure, so the same package solved with the
    same build_deps in different targets shares one layer. The build NODE's
    [universe] field is computed from the transitive doc-deps closure, so two
    solutions that agree on build_deps but disagree on doc_deps produce distinct
    [Build.t] records (same hash, distinct universe) — the doc DAG keys
    compile/link/doc-all hashes off [universe], which keeps each doc-side
    identity from contaminating the others. *)
