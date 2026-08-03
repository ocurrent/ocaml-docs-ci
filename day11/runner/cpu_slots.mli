(** NUMA-aware CPU slot pool for bounding container concurrency.

    A fixed set of slots is created at startup, each pinned to a specific subset
    of one NUMA node's CPUs. Memory is not pinned: first-touch from the pinned
    CPUs keeps a build's pages mostly node-local anyway, and a hard
    [cpuset.mems] binding turns one node filling up into a cpuset-constrained
    OOM kill even when other nodes have memory free. Build dispatch code calls
    {!acquire} before launching a container and {!release} after; acquire blocks
    the current fiber when every slot is in use.

    Each slot's [(cpuset, numa_mems)] pair threads straight into
    {!Day11_container.Oci_spec.make}'s [?cpuset] / [?numa_mems] parameters,
    which produces [linux.resources.cpu.{cpus,mems}] in the OCI config. runc
    then enforces the pin via cgroup v2 [cpuset] controllers, so [nproc] inside
    the container reports the slot size and nested [make -j$(nproc)]
    self-limits.

    {1 Detection}

    {!auto} reads [/sys/devices/system/node/node*/cpulist]. If the host has no
    NUMA layout (single-socket or [cpulist] unreadable), slots are laid out from
    [/proc/cpuinfo] with [numa_mems = None].

    {1 Layout}

    For [cores_per_build = N]:
    - Each NUMA node's CPU list is split into disjoint chunks of [N] CPUs. Any
      tail CPUs (fewer than [N]) are dropped.
    - Slots are emitted per-node, in [node -> chunk] order. *)

type t

type slot = {
  cpuset : string;
      (** Cpuset spec, e.g. ["0-3"] or ["0-1,20-21"]. Passed directly to
          {!Day11_container.Oci_spec.make}'s [?cpuset]. *)
  numa_mems : string option;
      (** NUMA node(s) this slot's memory is pinned to, if any. *)
  node : int;
      (** The originating NUMA node index. Useful for per-node scheduling
          decisions or logging. *)
}

val auto : ?overcommit:float -> cores_per_build:int -> unit -> t
(** Auto-detect the host NUMA layout and build a slot pool.

    @param cores_per_build
      Size of each cpuset in cpus. Slots pack into each NUMA node's cpu list in
      disjoint chunks of this size; tail cpus shorter than one chunk are
      dropped.

    @param overcommit
      Multiplier on the base slot count. [1.0] (default) gives the strict
      CPU-bounded pool: a build never shares cpus with another build. Values
      [> 1.0] replicate cpusets in the pool so multiple builds can land on the
      same cpuset and the kernel time-slices the cpus between them — useful when
      builds are I/O-bound (network fetches, disk I/O during opam-install) and
      CPU cores sit idle waiting. Rounded to the nearest integer slot count
      (minimum 1 slot). Values [< 1.0] are clamped to [1.0].

    Fails by [invalid_arg] if [cores_per_build < 1] or the host has fewer cpus
    than one chunk. *)

val n_slots : t -> int
(** Number of slots available — typically used to size the caller's own
    concurrency pool so it never queues more builds than slots. *)

val cores_per_build : t -> int
(** Returns the [cores_per_build] value passed to {!auto}. *)

val acquire : t -> slot
(** Reserve a slot. Blocks the current fiber if the pool is empty. *)

val release : t -> slot -> unit
(** Return a slot to the pool, unblocking one waiting caller. *)

val with_slot : t -> (slot -> 'a) -> 'a
(** [with_slot t f] runs [f] while holding a slot, releasing even if [f] raises.
*)

val describe : t -> string
(** One-line summary of the pool layout for startup logging. *)
