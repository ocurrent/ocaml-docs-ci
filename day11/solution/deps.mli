(** Dependency graph operations.

    Pure functions on dependency maps where each package maps to its direct
    dependencies. No solver or I/O dependencies. *)

type t = OpamPackage.Set.t OpamPackage.Map.t
(** A dependency graph: maps each package to its direct dependencies. *)

val transitive_deps : t -> t
(** [transitive_deps deps] enriches the map so each package maps to its full
    transitive dependency closure. *)

val has_cycle : t -> bool
(** [has_cycle deps] is [true] iff [deps] contains a dependency cycle (e.g.
    [ppxlib → ppxlib_jane → ppxlib] in the oxcaml overlay's patched
    [ppxlib.0.33.0+ox]). Useful for filtering solver output before passing to
    [build_dag], which can't build cyclic targets sensibly. *)

val extract_ocaml_version : t -> OpamPackage.t option
(** [extract_ocaml_version deps] finds [ocaml-base-compiler], [ocaml-variants],
    or [ocaml] in the graph. Returns the first match found. *)
