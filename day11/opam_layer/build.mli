(** In-memory build DAG node for an opam package layer.

    {!t} is the recursive type used by the planner, executor, and cache lookup
    paths to represent one opam package build. Each node carries:

    - a content-addressed [hash] computed from the base image, the transitive
      dep hashes, and the package's effective opam file
    - the [pkg] being built
    - a [deps] list — the *direct* dependency build nodes (which themselves
      carry their own [deps], so the field forms a DAG)
    - a [universe] identifier so that two builds of the same package against
      different sets of co-installed packages get distinct cache entries

    Use {!dir_name} or {!dir} to derive the on-disk path for a node; those wrap
    {!Day11_layer.Dir} which encodes the [build-XXXXXXXXXXXX] convention. *)

type t = {
  hash : string;
  pkg : OpamPackage.t;
  deps : t list;
  universe : Day11_solution.Universe.t;
}

val dir_name : t -> string
(** [dir_name b] returns the layer directory name for [b] (e.g.
    ["build-c9f7404f9f87"]). *)

val dir : os_dir:Fpath.t -> t -> Fpath.t
(** [dir ~os_dir b] returns the absolute layer directory under [os_dir]. *)

val layer : os_dir:Fpath.t -> t -> Day11_layer.Layer.t
(** [layer ~os_dir b] returns a {!Day11_layer.Layer.t} for [b]. *)
