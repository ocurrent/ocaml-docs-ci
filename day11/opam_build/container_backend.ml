let src = Logs.Src.create "day11.build.container_backend"
  ~doc:"Container-based opam package build backend"
module Log = (val Logs.src_log src)

module Build = Day11_opam_layer.Build
module Layer = Day11_layer.Layer

let mkdir path =
  Bos.OS.Dir.create ~path:true path |> ignore

(** Standard cleanup for opam-build layers. *)
let opam_build_cleanup ~sw env upper =
  let switch_dir = Fpath.(upper / "home" / "opam" / ".opam"
                          / Types.switch / ".opam-switch") in
  ignore (Day11_sys.Sudo.run ~sw env
    Bos.Cmd.(v "rm" % "-rf"
             % Fpath.to_string Fpath.(switch_dir / "sources")
             % Fpath.to_string Fpath.(switch_dir / "build")
             % Fpath.to_string Fpath.(switch_dir / "packages" / "cache")
             % Fpath.to_string Fpath.(upper / "tmp")));
  ignore (Day11_sys.Sudo.run ~sw env
    Bos.Cmd.(v "sh" % "-c"
             % Printf.sprintf "rm -f %s"
                 (Fpath.to_string
                    Fpath.(upper / "home" / "opam" / ".opam"
                           / "repo" / "state-*.cache"))))

let opam_build_strategy ?patches pkg =
  let pkg_str = OpamPackage.to_string pkg in
  let patch_args = match patches with
    | Some p when Patches.has_patches p pkg ->
      " " ^ Patches.patch_args p pkg
    | _ -> ""
  in
  (* TEMPORARY DIAGNOSTIC: run opam-build *under* strace from launch (not
     attach-after — the container lacks CAP_SYS_PTRACE, but tracing one's
     own child is allowed). [-ttt -T] gives absolute timestamps + per-call
     duration so the ~3.5s "load switch state" stall shows up as a single
     long syscall (or a wait4 on a slow child). The trace lands at
     /home/opam/strace.log, captured into the layer's fs/ for inspection.
     No [-f]: keep it to opam-build's main process (the switch load is
     in-process; a slow child still surfaces as a long wait4 gap). Falls
     back to a plain run if strace can't be installed, so builds never
     break. Revert to the plain [opam-build -v ...] once diagnosed. *)
  { Types.cmd = Printf.sprintf "opam-build -v %s%s" pkg_str patch_args;
    cleanup = opam_build_cleanup }

(** Default container env for opam-build containers.

    NB on Eio programs run in these containers (odoc_driver_voodoo in
    the doc stages): at very high container concurrency (~480) the
    default io_uring backend can fail at startup with ENOMEM from
    io_uring_queue_init — a fast, retryable failure. Forcing
    EIO_BACKEND=posix avoids that but trades it for a rare missed-
    SIGCHLD hang in eio_posix's child reaping that wedges executor
    slots permanently — strictly worse. So: stay on io_uring and keep
    container concurrency moderate. *)
let opam_container_env = [
  ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
  ("HOME", "/home/opam");
]

(** Build an OCI spec template for an opam-build container running
    [cmd] via [bash -c]. [?cpuset] and [?numa_mems] pin the container
    to a subset of host CPUs / NUMA memory nodes via cgroups, used by
    {!Day11_runner.Cpu_slots} to cap nested build parallelism.

    [?jobs] additionally exports [OPAMJOBS], capping opam's [%{jobs}%]
    variable. The cpuset alone is not enough: opam derives its default
    jobs from the *online* CPU count (sysconf), which ignores cpuset
    affinity — so a build pinned to 4 cpus would still run e.g.
    [dune -j 767] on a 768-core host, multiplying its memory
    footprint by the spawned-compiler count. *)
let opam_build_spec ?cpuset ?numa_mems ?jobs ~cmd ~mounts ~uid ~gid
    () : Day11_container.Oci_spec.t =
  let env = match jobs with
    | None -> opam_container_env
    | Some n -> ("OPAMJOBS", string_of_int n) :: opam_container_env
  in
  Day11_container.Oci_spec.make
    ~cwd:"/home/opam"
    ~hostname:"builder"
    ~env
    ~mounts
    ~network:true
    ?cpuset ?numa_mems
    ~argv:[ "/usr/bin/env"; "bash"; "-c"; cmd ]
    ~uid ~gid
    ()

(** Default pre-mount prep for opam-build containers. *)
let opam_build_prep_upper ~sw env ~uid ~gid ~upper ~lowers =
  let switch_rel = Fpath.(v "home" / "opam" / ".opam" / Types.switch
                          / ".opam-switch") in
  let packages_rel = Fpath.(switch_rel / "packages") in
  let packages_dirs = List.filter_map (fun dir ->
    let p = Fpath.(dir // packages_rel) in
    if Bos.OS.Dir.exists p |> Result.get_ok then Some p else None
  ) lowers in
  if packages_dirs <> [] then begin
    let state_dir = Fpath.(upper // switch_rel) in
    mkdir state_dir;
    Day11_opam_layer.Opamh.dump_state packages_dirs
      Fpath.(state_dir / "switch-state") |> ignore
  end;
  let home_dir = Fpath.(upper / "home") in
  if Bos.OS.Dir.exists home_dir |> Result.get_ok then
    ignore (Day11_sys.Sudo.run ~sw env
      Bos.Cmd.(v "chown" % "-R" % Printf.sprintf "%d:%d" uid gid
               % Fpath.to_string home_dir))

(** Collect the transitive dep layer hashes of a build node, in the
    order they are stacked as overlayfs lowers.

    Returned in deterministic DFS post-order: every dep appears after
    all its own deps, with duplicates removed by first occurrence.
    The list is then reversed so direct deps sit at the front —
    those become the topmost lowers in overlayfs, matching the
    dependency ordering the DAG was built with.

    The previous implementation used [Hashtbl.fold], which gives
    undefined iteration order: two runs of the same node could stack
    the layers differently, so the visible version of single-file-
    across-layers things like [/var/lib/dpkg/status] was
    non-deterministic. For lablgl builds in particular this made
    opam alternately report [libglu1-mesa-dev] as installed or
    uninstalled depending on whichever layer's [dpkg/status] ended
    up topmost — leading to flaky build outcomes.

    This is the source of truth for the lower-stack order. It is
    recorded verbatim in [build.json] ([Build_meta.t.stack]) so a tool
    replaying the build reconstructs the identical rootfs without
    re-deriving the DAG ordering. *)
let collect_transitive_dep_hashes (node : Build.t) =
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  let rec walk (b : Build.t) =
    if not (Hashtbl.mem seen b.hash) then begin
      Hashtbl.replace seen b.hash ();
      List.iter walk b.deps;
      acc := b.hash :: !acc
    end
  in
  List.iter walk node.deps;
  List.rev !acc

(** Collect transitive dep layer dirs from a build node, in
    overlay-stack order. The dir-level view of
    {!collect_transitive_dep_hashes}. *)
let collect_transitive_dep_dirs ~os_dir (node : Build.t) =
  List.map (fun hash -> Layer.dir (Layer.of_hash ~os_dir hash))
    (collect_transitive_dep_hashes node)

(** Transitive dependency packages of a build node (the full closure,
    deduped). The per-package repo slice mounted at [repo/default] must
    contain {b every} package opam will see as installed — i.e. [node.pkg]
    plus this closure — otherwise opam's switch-state load fails with
    "No definition found for the following installed packages" (it looks
    up each installed package's opam definition in the repo). A 1-package
    slice is too narrow; the full ~18k-package repo is too broad (slow). *)
let collect_transitive_dep_pkgs (node : Build.t) =
  let seen = Hashtbl.create 16 in
  let acc = ref [] in
  let rec walk (b : Build.t) =
    if not (Hashtbl.mem seen b.hash) then begin
      Hashtbl.replace seen b.hash ();
      List.iter walk b.deps;
      acc := b.pkg :: !acc
    end
  in
  List.iter walk node.deps;
  List.rev !acc

let build ~sw env (benv : Types.build_env)
    ~opam_repositories ?(mounts = [])
    ?patches ?build_dirs ?prep_upper
    ?strategy (node : Build.t) ~target_fs () =
  let os_dir = benv.os_dir in
  let strategy = match strategy with
    | Some s -> s
    | None -> opam_build_strategy ?patches node.pkg
  in
  let dep_dirs = match build_dirs with
    | Some dirs -> dirs
    | None -> collect_transitive_dep_dirs ~os_dir node
  in
  let base_prep_upper = match prep_upper with
    | Some f -> f
    | None -> opam_build_prep_upper ~sw env ~uid:benv.uid ~gid:benv.gid
  in
  (* The container runs with hostname "builder" (see [opam_build_spec]).
     runc — unlike [docker run] — does NOT populate [/etc/hosts], and the
     base image's is an empty file, so resolving "builder" (opam and sudo
     both do) misses [files] and falls through to DNS. We give it a local
     entry.

     We write it into the overlay's writable UPPER, NOT as a bind-mount
     onto [/etc/hosts]. That path exists only in the read-only lower (the
     base's 0-byte file), so bind-mounting onto it forces overlayfs to
     copy the file up to create a mount target — and that copy-up races
     runc 1.1.x's hardened mount-target re-check: if it interleaves, runc
     sees the original dentry as "/etc/hosts (deleted)" and aborts the
     whole container with "possibly malicious path detected". Rare (~0.2%
     of builds) but real, and clustered under concurrency. A plain regular
     file in the upper shadows the lower with no copy-up and no mount. *)
  let prep_upper ~upper ~lowers =
    base_prep_upper ~upper ~lowers;
    let etc = Fpath.(upper / "etc") in
    mkdir etc;
    ignore (Bos.OS.File.write Fpath.(etc / "hosts")
              "127.0.0.1\tlocalhost builder\n")
  in
  let repo_mounts =
    if opam_repositories = [] then []
    else
      let temp = Bos.OS.Dir.tmp "day11_repo_%s" |> Result.get_ok in
      match Day11_opam_layer.Opam_repo.create temp with
      | Ok repo_dir ->
          (* Slice must hold [node.pkg] AND its whole installed-dep
             closure — opam resolves every installed package's definition
             against the repo at switch-state load. *)
          let pkgs = node.pkg :: collect_transitive_dep_pkgs node in
          let _ = Day11_opam_layer.Opam_repo.populate ~opam_repo:repo_dir
            ~opam_repositories pkgs in
          [ Day11_container.Mount.bind_ro
              ~src:(Fpath.to_string repo_dir)
              "/home/opam/.opam/repo/default" ]
      | Error _ -> []
  in
  let patch_mounts = match patches with
    | Some p when Patches.has_patches p node.pkg ->
      let patch_files = Patches.patches_for p node.pkg in
      List.mapi (fun i src ->
        Day11_container.Mount.bind_ro ~src
          (Printf.sprintf "/patches/%03d.patch" i)
      ) patch_files
    | _ -> []
  in
  let all_mounts = repo_mounts @ patch_mounts @ mounts in
  (* Acquire a CPU slot if the env has a pool; fibre-blocks until one
     is free. Within the slot's lifetime, nproc inside the container
     reports the slot size, so nested [make -j$(nproc)] self-limits. *)
  let run_in_slot f = match benv.Types.cpu_slots with
    | None -> f ~cpuset:None ~numa_mems:None ~jobs:None
    | Some pool ->
      let jobs = Day11_runner.Cpu_slots.cores_per_build pool in
      Day11_runner.Cpu_slots.with_slot pool (fun slot ->
        f ~cpuset:(Some slot.cpuset) ~numa_mems:slot.numa_mems
          ~jobs:(Some jobs))
  in
  run_in_slot @@ fun ~cpuset ~numa_mems ~jobs ->
  let spec = opam_build_spec ?cpuset ?numa_mems ?jobs
    ~cmd:strategy.Types.cmd
    ~mounts:all_mounts ~uid:benv.uid ~gid:benv.gid ()
  in
  match Day11_runner.Run_in_layers.run ~sw env ~base:benv.base
          ~build_dirs:dep_dirs ~prep_upper spec with
  | Ok (run, upper, timing) ->
    strategy.cleanup ~sw env upper;
    (* Strip overlayfs [trusted.overlay.opaque] xattrs from the
       captured upper. The kernel places this xattr on directories
       that the container modified in a way that replaces the lower
       contents (e.g. apt remove + reinstall, or mkdir after rmdir).
       It is meaningful WITHIN the build overlay but actively harmful
       once the upper is promoted to a layer [fs/] — when that layer
       is later stacked as a lower in another build, the opaque
       marker silently shadows sibling lowers' contents for that
       directory. See lablgl + conf-libglu bug: conf-libgl's
       captured [/usr/include/GL] had opaque=y, hiding [glu.h] from
       [conf-libglu] whenever conf-libgl sorted above it in
       [lowerdir], and causing non-deterministic build failures
       depending on iteration order of [collect_transitive_dep_dirs].

       [setfattr -x] fails if the attribute isn't present, so filter
       via [getfattr -R --match] first to get only the set. *)
    (* [-m] lists only dirs that HAVE the attr (no per-file "no such
       attribute" noise, unlike [-n]), and [pipefail] makes a missing
       [getfattr]/[setfattr] (the [attr] package) surface as an error
       instead of a silent no-op — which is how this strip quietly did
       nothing for months, letting opaque markers survive into layers
       and shadow sibling lowers (e.g. conf-libX11's [/usr/include/X11]
       hiding conf-libXft's [Xft/], failing every graphics build). *)
    let () =
      match Day11_sys.Sudo.run ~sw env
        Bos.Cmd.(v "bash" % "-c"
          % Printf.sprintf
              "set -o pipefail; \
               getfattr -h -R -m trusted.overlay.opaque --absolute-names %s \
               | awk '/^# file:/ {print $3}' | \
               xargs -r -I{} setfattr -x trusted.overlay.opaque {}"
              (Filename.quote (Fpath.to_string upper)))
      with
      | Ok _ -> ()
      | Error (`Msg m) ->
        Log.err (fun f -> f "opaque-xattr strip failed for %a (is the \
          'attr' package installed?): %s" Fpath.pp target_fs m)
    in
    let _ = Day11_sys.Sudo.run ~sw env
      Bos.Cmd.(v "mv" % Fpath.to_string upper
               % Fpath.to_string target_fs) in
    ignore (Day11_sys.Sudo.rm_rf ~sw env (Fpath.parent upper));
    (* Post-build invariant: a package classified as a real compiler
       by [Compiler_pkg.is_compiler] must have left
       [lib/ocaml/stdlib.cmti] in its build layer. Logging at error
       level (rather than failing the build) so the diagnostic is
       visible without blocking unrelated work — a misclassified
       package shouldn't take down the whole pipeline. *)
    let build_layer = Fpath.parent target_fs in
    (match Compiler_pkg.check_stdlib_installed
             ~build_layer node.pkg with
     | Ok () -> ()
     | Error (`Msg e) -> Log.err (fun m -> m "%s" e));
    Ok (run, timing)
  | Error (`Msg e) as err ->
    Log.err (fun m -> m "Container build failed for %s: %s"
      (OpamPackage.to_string node.pkg) e);
    err
