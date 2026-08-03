(** Determine whether a package needs separate compile and link phases.

    Compares a package's dependencies in the build graph (no [{post}] deps)
    versus the doc graph (with [{post}] deps and [x-extra-doc-deps]). If they
    differ, the package needs separate compile and link phases; the link phase
    needs odoc output from the extra deps that aren't available at compile time.

    See {!page-doc_dep_graphs} for the full description of the two graphs and
    what each pipeline step consumes. *)

val needs_separate_link : Day11_solution.Solve_result.t -> OpamPackage.t -> bool
(** [needs_separate_link result pkg] returns [true] when [pkg]'s dependencies
    differ between [result.build_deps] and [result.doc_deps]. *)
