(** OCI runtime specification (config.json) generation.

    {{:https://github.com/opencontainers/runtime-spec}runc} and other OCI
    runtimes read a bundle's [config.json] to decide everything about how the
    container is launched: what to run, in what rootfs, with what namespaces,
    capabilities, mounts, seccomp filters, and so on.

    {1 Spec templates}

    A {!t} value is a {b spec template} — a fully-described container
    parameterized only by the rootfs path. Every other field (cwd, env, argv,
    mounts, namespaces, capabilities, …) is baked in when the template is
    constructed via {!make}.

    Templates exist because the rootfs path is typically only known {e after} a
    host-side mount step (overlayfs, bind mount, chroot setup, …). The caller
    knows what container they want well in advance; the rootfs path is filled in
    last by the code that physically performs the mount and then calls
    {!to_yojson} or {!write}.

    {1 Defaults}

    Most of the OCI spec is boilerplate that never varies between builds.
    {!make} hardcodes sensible values for all of it:

    - Linux namespaces: [pid], [ipc], [uts], [mount], and [network] (unless
      [~network:true] is passed).
    - Capabilities: a Docker-equivalent baseline (CAP_CHOWN, CAP_DAC_OVERRIDE,
      CAP_FSETID, CAP_FOWNER, CAP_MKNOD, CAP_SETGID, CAP_SETUID, CAP_SETFCAP,
      CAP_SETPCAP, CAP_SYS_CHROOT, CAP_KILL, CAP_AUDIT_WRITE).
    - Seccomp: [fsync], [fdatasync], [msync], [sync], [syncfs], and
      [sync_file_range] are all masked to return [ERRNO 0]. This is deliberate —
      it makes opam package installs significantly faster because the build
      doesn't need durability guarantees inside an ephemeral container.
    - [rlimits]: [RLIMIT_NOFILE] = 1024.
    - Standard system mounts: [/proc], [/sys], [/dev], [/dev/pts], [/dev/shm],
      [/dev/mqueue], [/sys/fs/cgroup], and [/tmp].
    - Masked and read-only [/proc] subpaths matching the Docker defaults.
    - If [~network:true]: a bind-mount of [/etc/resolv.conf] so DNS works inside
      the container.

    What varies per call is: what command to run, where, as whom, and with which
    extra mounts. *)

type t
(** A spec template — a fully-described container missing only the rootfs path.
    Construct with {!make}, instantiate with {!to_yojson} or {!write}. *)

val make :
  ?terminal:bool ->
  ?cwd:string ->
  ?hostname:string ->
  ?env:(string * string) list ->
  ?mounts:Mount.t list ->
  ?network:bool ->
  ?cpuset:string ->
  ?numa_mems:string ->
  argv:string list ->
  uid:int ->
  gid:int ->
  unit ->
  t
(** [make ~argv ~uid ~gid ()] builds a spec template. The rootfs path is
    supplied later via {!to_yojson} or {!write}.

    @param argv
      Command and arguments. First element is the executable, the rest are
      passed to it unchanged.
    @param uid User ID inside the container.
    @param gid Group ID inside the container.
    @param terminal Whether to allocate a controlling TTY. Default [false].
    @param cwd Working directory inside the container. Default ["/"].
    @param hostname
      Hostname visible inside the container. Default ["container"].
    @param env
      Environment variables as [(key, value)] pairs. Serialized as [KEY=value]
      strings. Default [[]].
    @param mounts
      Extra bind mounts on top of the standard system mounts (which are added
      automatically). Default [[]].
    @param network
      If [true], the container joins the host network namespace and gets
      [/etc/resolv.conf] bind-mounted. If [false] (the default), the container
      is in its own network namespace and has no connectivity.
    @param cpuset
      Restrict the container to a set of host CPUs. Emitted as
      [linux.resources.cpu.cpus] (cgroup [cpuset.cpus]). Format is the Linux
      cpuset list: e.g. ["0-3"] or ["0-3,20-23"]. When set, [nproc] inside the
      container reports the cpuset count.
    @param numa_mems
      Restrict the container to memory from these NUMA nodes. Emitted as
      [linux.resources.cpu.mems] (cgroup [cpuset.mems]). Format is the Linux
      nodeset: e.g. ["0"] or ["0-1"]. Pairs naturally with [?cpuset] to keep
      memory local to the cpus running the build. *)

val to_yojson : root:string -> t -> Yojson.Safe.t
(** [to_yojson ~root t] instantiates the template with [root] as the container's
    rootfs path and returns the JSON object that runc expects in [config.json].
*)

val write : root:string -> Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [write ~root bundle_dir t] instantiates [t] with [root] and writes it as
    [bundle_dir/config.json], where {{!Runc.run}runc} expects to find it. *)

val placeholder_root : string
(** A sentinel string ([<rootfs>]) used by {!write_template} as the rootfs path.
    Callers reading back a template-mode [config.json] are expected to
    substitute it for a real overlay-merged rootfs before invoking runc. *)

val write_template : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [write_template path t] writes [t] to [path] (typically
    [<layer_dir>/config.json]) with {!placeholder_root} as the rootfs path. The
    result is a valid OCI bundle descriptor — once a host substitutes the
    placeholder for a real rootfs path (e.g. an overlay merging the base image
    with the layer's transitive deps) the bundle is directly runc-runnable. *)

val instantiate_template :
  root:string -> Yojson.Safe.t -> (Yojson.Safe.t, [> Rresult.R.msg ]) result
(** [instantiate_template ~root json] takes a template-mode [config.json] (as
    produced by {!write_template}) and returns it with {!placeholder_root}
    replaced by the real rootfs path [root]. This is the inverse half of
    {!write_template}: it lets a tool replay a recorded bundle without
    re-deriving the spec. Since a template is concrete JSON parameterized only
    by [root.path], this substitutes that one field rather than parsing back
    into a {!t}. Errors if [json] isn't a recognisable template (no [root.path],
    or [root.path] isn't {!placeholder_root} - e.g. already instantiated). *)

val instantiate_template_file :
  template:Fpath.t ->
  root:string ->
  bundle_dir:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [instantiate_template_file ~template ~root ~bundle_dir] reads the template
    [config.json] at [template], substitutes [root] for {!placeholder_root} (via
    {!instantiate_template}), and writes the result as [bundle_dir/config.json]
    \- a bundle directly runnable by {{!Runc.run}runc}. *)
