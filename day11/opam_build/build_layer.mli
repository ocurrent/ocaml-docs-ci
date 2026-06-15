(** Build one layer via {!Container_backend}.

    Handles the backend-agnostic parts of the build lifecycle: cache lookup,
    layer locking, writing the generic [layer.json], calling [on_extract] so
    callers can drop domain-specific sidecars. {!Container_backend} (a
    {!Backend.S}) owns the part that differs: stacking deps, running the build,
    capturing the filesystem diff. *)

(** {1 Container-backend helpers}

    These used to live directly in {!Build_layer}. They are now
    container-specific primitives in {!Container_backend} and re-exported here
    for backward compatibility — callers that build a custom [?strategy] on top
    of [opam-build] typically compose with [opam_build_cleanup], etc. *)

val opam_build_cleanup :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Fpath.t -> unit
(** See {!Container_backend.opam_build_cleanup}. *)

val opam_build_spec :
  ?cpuset:string ->
  ?numa_mems:string ->
  ?jobs:int ->
  cmd:string ->
  mounts:Day11_container.Mount.t list ->
  uid:int ->
  gid:int ->
  unit ->
  Day11_container.Oci_spec.t
(** See {!Container_backend.opam_build_spec}. *)

val opam_build_prep_upper :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  uid:int ->
  gid:int ->
  upper:Fpath.t ->
  lowers:Fpath.t list ->
  unit
(** See {!Container_backend.opam_build_prep_upper}. *)

(** {1 Main entry point} *)

val build :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Types.build_env ->
  opam_repositories:Fpath.t list ->
  ?snapshot_repos:Fpath.t list ->
  ?mounts:Day11_container.Mount.t list ->
  ?patches:Patches.t ->
  ?build_dirs:Fpath.t list ->
  ?prep_upper:(upper:Fpath.t -> lowers:Fpath.t list -> unit) ->
  ?on_extract:(layer_dir:Fpath.t -> success:bool -> unit) ->
  Day11_opam_layer.Build.t ->
  ?strategy:Types.build_strategy ->
  unit ->
  Types.build_result
(** [build ~sw env benv ?... node ()] builds [node] via {!Container_backend},
    writes its generic [layer.json], and calls [on_extract] so the caller can
    write any domain-specific sidecar files.

    @param opam_repositories
      Repo source paths the backend extracts a one-package slice from to mount
      as the container's [default] repo. Required (see
      {!Container_backend.build}): pass [[]] only to deliberately fall back to
      the base image's full opam-repository.
    @param snapshot_repos
      Source repositories from which to take a one-package opam-repository slice
      for the layer dir's [opam-repository/] subdir. Defaults to
      [opam_repositories] when not given. Pass [Some []] to skip slice writing
      altogether. The resulting slice plus the layer's [fs/] make every
      successful layer self-describing for cold-storage rerun.
    @param build_dirs
      Override the dep layer directories stacked as overlay lowers. By default
      ([None]), collects the transitive deps of [node] from the cache.
    @param prep_upper
      Override the pre-mount prep callback. Container backend default: dump opam
      switch-state + chown [/home]. Ignored by non-container backends.
    @param on_extract
      Called after a build's [fs/] has been moved into place and [layer.json]
      written, with [success:true] iff exit status was 0. NOT called on cache
      hits.

    Default strategy is [opam-build -v <pkg>]. *)
