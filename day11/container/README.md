# container — OCI container runtime primitives

Thin wrappers around Linux overlayfs and [runc](https://github.com/opencontainers/runc)
for running isolated commands in a layered rootfs. This is the lowest
level at which day11 touches the kernel: it knows how to mount
filesystems, build an OCI runtime spec, and launch/delete a container
— nothing more.

No domain knowledge. This library does not know about opam packages,
layers in the day11 sense, caching, doc generation, or builds. It is
composed by `day11/opam_build` (via `day11/runner`) to run one
container per package build.

## External dependencies

- `day11_sys` — sudo wrappers
- `yojson` — config.json generation
- `bos`, `fpath`, `rresult` — filesystem/error helpers

Does NOT depend on `day11_layer`, `day11_opam`, `opam-format`, or any
solver/build libraries.

## Modules

### `Mount` — bind-mount specs for application inputs

```ocaml
type t = { ty : string; src : string; dst : string; options : string list }
val to_json : t -> Yojson.Safe.t
val bind_ro : src:string -> string -> t
val bind_rw : src:string -> string -> t
```

`Mount.t` values are passed to `Oci_spec.make` via `~mounts`. The
system mounts every container needs (`/proc`, `/sys`, `/dev/*`,
`/tmp`) are added automatically by `Oci_spec.make` — callers only
supply application-specific bind mounts (opam repo overlay, patches,
odoc output trees, etc.).

### `Oci_spec` — generate runc's config.json

```ocaml
val make :
  ?terminal:bool -> ?cwd:string -> ?hostname:string ->
  ?env:(string * string) list -> ?mounts:Mount.t list -> ?network:bool ->
  root:string -> argv:string list -> uid:int -> gid:int ->
  unit ->
  Yojson.Safe.t

val write : Fpath.t -> Yojson.Safe.t -> (unit, [> Rresult.R.msg]) result
```

`make` is a pure function that builds an OCI runtime spec. Nearly
everything in that spec is boilerplate, hardcoded here with sensible
defaults:

- Linux namespaces: pid, ipc, uts, mount (+ network when
  `~network:false`).
- Capabilities: Docker's baseline (CAP_CHOWN, CAP_DAC_OVERRIDE,
  CAP_FSETID, CAP_FOWNER, CAP_MKNOD, CAP_SETGID, CAP_SETUID,
  CAP_SETFCAP, CAP_SETPCAP, CAP_SYS_CHROOT, CAP_KILL, CAP_AUDIT_WRITE).
- Seccomp: `fsync`, `fdatasync`, `msync`, `sync`, `syncfs`,
  `sync_file_range` all return `ERRNO 0`. This is a deliberate
  performance hack — build containers don't need durability.
- System mounts: `/proc`, `/sys`, `/dev`, `/dev/pts`, `/dev/shm`,
  `/dev/mqueue`, `/sys/fs/cgroup`, `/tmp`.
- `rlimits`: `RLIMIT_NOFILE = 1024`.

`write` saves the spec as `<bundle>/config.json`. Split from `make`
so callers can inspect or tweak the spec before writing.

### `Overlay` — overlayfs mount/umount

```ocaml
val mount : Eio_unix.Stdenv.base ->
  lower:Fpath.t list -> upper:Fpath.t -> work:Fpath.t ->
  target:Fpath.t -> (unit, [> Rresult.R.msg]) result

val umount : Eio_unix.Stdenv.base -> Fpath.t ->
  (unit, [> Rresult.R.msg]) result
```

Shells out to `sudo mount -t overlay` and `sudo umount`. The `lower`
list uses the kernel's convention: leftmost entry is the topmost
layer, so files in `lower.(0)` shadow files in `lower.(1)` shadow
files in `lower.(2)`. Directories are merged across all lowers.

### `Runc` — container run/delete

```ocaml
val run : Eio_unix.Stdenv.base ->
  bundle:Fpath.t -> container_id:string ->
  (Day11_exec.Run.t, [> Rresult.R.msg]) result

val delete : Eio_unix.Stdenv.base -> string ->
  (unit, [> Rresult.R.msg]) result
```

`run` shells out to `sudo runc run -b <bundle> <container_id>` and
blocks until the container exits. Captures stdout/stderr to
`<bundle>/runc.log` as a side effect. The returned
`Day11_exec.Run.t` carries the container's exit status in its
`status` field.

`delete` runs `sudo runc delete -f` to clean up runc's on-disk
registry entry. Callers typically pair `run` with `delete` inside a
`Fun.protect` so a failed run doesn't leak state.

## A typical caller

The call order inside `day11/runner/run_in_layers.ml`:

```ocaml
Overlay.mount env ~lower ~upper ~work ~target:merged;
Fun.protect
  ~finally:(fun () -> ignore (Overlay.umount env merged))
  (fun () ->
    let spec = Oci_spec.make
      ~cwd:"/home/opam"
      ~hostname:"builder"
      ~env:container_env
      ~mounts
      ~network:true
      ~root:(Fpath.to_string merged)
      ~argv:["/usr/bin/env"; "bash"; "-c"; cmd]
      ~uid ~gid
      ()
    in
    Oci_spec.write bundle_dir spec;
    let container_id = "day11-" ^ ... in
    ignore (Runc.delete env container_id);  (* stale cleanup *)
    Fun.protect
      ~finally:(fun () -> ignore (Runc.delete env container_id))
      (fun () -> Runc.run env ~bundle:bundle_dir ~container_id))
```

## Testing

### Unit tests

`test/test_container.ml` runs under `dune test` without root. It
covers:

- `Mount.to_json`, `Mount.bind_ro`, `Mount.bind_rw`
- `Oci_spec.make` with various combinations (basic spec, env,
  seccomp, network on/off, mounts, capabilities, terminal flag)
- `Oci_spec.write` — verify the JSON round-trips to disk

No actual mounts or containers.

### Integration tests

`test/test_integration.ml` and `test/test_build_package.ml` require
Linux + runc + sudo + a statically-linked busybox. They're gated on
the environment variable:

```
DAY11_INTEGRATION=true dune exec day11/container/test/test_integration.exe
```

These actually mount overlays and run containers end to end. They
cover:

- runc echo — smoke test
- overlay + runc — stack layers, read a file from a lower
- Hybrid lowerdir plan — exercises
  `Day11_layer.Stack.plan_lowerdir` with varying dep counts
  (pure multi-lower, forced split with small budget, 4K boundary)
  and verifies every dep's file is visible inside the running
  container

The unit tests are the always-on signal; the integration tests are
the "does this actually work against the kernel" verification that
runs on demand.
