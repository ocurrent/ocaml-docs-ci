(** Container-based build backend.

    Stacks dep layers as an overlayfs and runs the build inside a
    runc container. This is the original day11 behaviour, factored
    out from {!Build_layer.build} so that a native-host alternative
    ({!Native_backend}) can be plugged into the same
    {!Backend.S} slot.

    The helpers re-exported here ({!opam_build_cleanup},
    {!opam_build_spec}, {!opam_build_prep_upper}) are the
    container-specific building blocks that callers compose into
    custom strategies — e.g. the doc pipeline passes its own
    strategy with [opam_build_cleanup] as the cleanup function. *)

val opam_build_cleanup :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Fpath.t -> unit
(** Remove [.opam-switch/build/], [sources/], [packages/cache],
    [/tmp], and [repo state-*.cache] from an upper dir. Suitable
    for any layer built with opam. *)

val opam_build_spec :
  ?cpuset:string ->
  ?numa_mems:string ->
  ?jobs:int ->
  cmd:string ->
  mounts:Day11_container.Mount.t list ->
  uid:int -> gid:int ->
  unit ->
  Day11_container.Oci_spec.t
(** Build an OCI spec template for a container that runs [cmd] via
    [bash -c], with the cwd / env / hostname / network defaults
    that opam-build expects.
    @param cpuset Restrict the container to these host CPUs (cgroup
      cpuset.cpus). Normally supplied from
      {!Day11_runner.Cpu_slots.acquire}.
    @param numa_mems Restrict the container's memory to these NUMA
      nodes (cgroup cpuset.mems). Pairs with [?cpuset].
    @param jobs Export [OPAMJOBS=jobs] so opam's [%{jobs}%] matches
      the cpuset size. opam's default comes from the online CPU
      count, which ignores cpuset affinity — without this a pinned
      build still spawns host-cpu-count compiler processes. *)

val opam_build_prep_upper :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> uid:int -> gid:int ->
  upper:Fpath.t -> lowers:Fpath.t list -> unit
(** Standard pre-mount prep for an opam-build container:
    dumps a synthetic opam [switch-state] file from the lowers'
    [.opam-switch/packages] directories into the upper, and chowns
    [upper/home] to [uid:gid]. *)

val opam_build_strategy :
  ?patches:Patches.t -> OpamPackage.t -> Types.build_strategy
(** Default build strategy for a package — runs
    [opam-build -v <pkg>] and cleans via {!opam_build_cleanup}. *)

val collect_transitive_dep_hashes :
  Day11_opam_layer.Build.t -> string list
(** Transitive dep layer hashes in overlayfs-stack order (direct deps
    frontmost / topmost). The hash-level core of
    {!collect_transitive_dep_dirs}; recorded verbatim in [build.json]
    ([Build_meta.t.stack]) so a tool can reconstruct the rootfs lower
    stack without re-deriving the DAG ordering. *)

val collect_transitive_dep_dirs :
  os_dir:Fpath.t -> Day11_opam_layer.Build.t -> Fpath.t list
(** Collect transitive dep layer dirs from a build node, in
    overlay-stack order. Used as the default [?build_dirs] when none
    is supplied. *)

val collect_transitive_dep_pkgs :
  Day11_opam_layer.Build.t -> OpamPackage.t list
(** Transitive dependency packages of a build node (the full closure,
    deduped, in overlay-stack order). The package-level view of
    {!collect_transitive_dep_hashes}. *)

include Backend.S
(** {1 Backend interface} *)
