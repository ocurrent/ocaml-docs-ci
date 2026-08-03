(** Generic per-layer metadata, serialized as [layer.json].

    Every layer of every kind has a [layer.json] file holding a {!t} record. The
    record contains only information that is meaningful for any layer regardless
    of what built it: how the container exited, which parents it stacked on, who
    the build user was, what time it took, when it was created.

    {1 Sidecar convention}

    Domain-specific information (the package being built, installed files,
    applied patches, odoc phase, etc.) lives in {b sidecar} files in the same
    directory and is owned by the relevant domain library. The "kind" of a layer
    is determined by which sidecar files are present:

    - [build.json] (see [Day11_opam_layer.Build_meta]) marks an opam package
      build layer
    - [doc.json] (see [Day11_opam_layer.Doc_meta]) marks an odoc doc layer
    - future kinds add their own sidecars

    {b Sidecar files MUST be valid JSON} (typically a single object at the top
    level). The generic library does not enforce this — no part of [day11_layer]
    reads sidecars — but tools that walk the cache rely on the convention to
    display layer contents without needing the relevant domain library linked
    in.

    LRU access bookkeeping is kept separate — see {!Last_used} — so touching a
    layer doesn't rewrite its JSON.

    {1 Phase timing}

    Build pipelines record per-phase timing as a free-form
    [(string * float) list]. Phase names are not validated; readers that want a
    specific well-known phase use {!timing_field}. *)

type timing = (string * float) list

val empty_timing : timing
val timing_field : string -> timing -> float

(** {1 Layer metadata} *)

type t = {
  exit_status : int;
      (** Exit status of the build process: 0 = success, anything else is a
          failure. *)
  parent_hashes : string list;
      (** Full hashes of the layers that were stacked as the lowers of the
          overlay during the build, in the order they were stacked. For domain
          types that also carry their own dep records (e.g. opam package
          strings), the parallel list lives in the sidecar. *)
  uid : int;
  gid : int;
  base_hash : string;
      (** Hash of the base image layer at the bottom of the overlay. *)
  disk_usage : int;  (** Approximate size in bytes of the layer's [fs/] tree. *)
  timing : timing;
  created_at : string;
      (** ISO-8601 UTC timestamp of when the layer was extracted. *)
  failed_dep : string option;
      (** [Some name] for layers that didn't run because a dependency build
          failed. [None] for layers that ran to completion (whether successfully
          or with a non-zero exit). *)
}

(** {1 Read and write} *)

val save :
  ?created_at:string ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  t ->
  (unit, [> Rresult.R.msg ]) result
(** [save env path t] writes [t] to [path] as JSON. If [?created_at] is
    supplied, it overrides [t.created_at]; if omitted and [t.created_at] is
    empty, the current UTC time is used. *)

val load : Eio_unix.Stdenv.base -> Fpath.t -> (t, [> Rresult.R.msg ]) result
(** [load env path] reads and parses a [layer.json]. *)
