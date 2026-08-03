(** Result of dependency solving.

    Carries both the build dependency graph (acyclic, used for topological build
    ordering) and the doc dependency graph (may contain cycles via
    [x-extra-doc-deps], used for odoc cross-referencing). Both graphs span the
    same package set. *)

type t = {
  packages : OpamPackage.Set.t;
      (** The version assignment chosen by the solver. *)
  build_deps : Deps.t;
      (** Acyclic. Each package mapped to the packages it needs to build and
          install. Safe to topologically sort. *)
  doc_deps : Deps.t;
      (** May contain cycles. Each package mapped to the packages whose odoc
          output it needs for cross-referencing. Equals {!build_deps} plus
          [{post}] deps and [x-extra-doc-deps] edges. Do NOT topologically sort
          this graph. *)
  examined : OpamPackage.Name.Set.t;
      (** Package names the solver touched during solving. Used for incremental
          cache invalidation: if none of the examined names changed between
          opam-repo commits, the cached result is still valid. *)
}

val to_json : t -> Yojson.Safe.t
(** Serialize a solve result to JSON. *)

val of_json : Yojson.Safe.t -> (t, [> Rresult.R.msg ]) result
(** Deserialize a solve result from JSON. *)
