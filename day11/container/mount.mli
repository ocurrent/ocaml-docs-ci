(** Mount point specifications for OCI containers.

    A {!t} value describes a single entry in the container's [mounts] array,
    exactly as it appears in the OCI runtime spec. Values are constructed
    directly or via the {!bind_ro}/{!bind_rw} helpers and collected into a list
    that is passed to {!Oci_spec.make}.

    The system mounts that every container needs ([/proc], [/sys], [/dev/pts],
    etc.) are injected automatically by {!Oci_spec.make} and should NOT appear
    in the caller's mount list — the caller is only responsible for
    application-specific mounts, which in practice means bind mounts. *)

type t = {
  ty : string;
      (** Mount type as understood by the kernel: ["bind"], ["tmpfs"], ["proc"],
          ["sysfs"], ["cgroup"], ["devpts"], ["mqueue"], etc. *)
  src : string;
      (** Source of the mount. For bind mounts this is the absolute path on the
          host filesystem; for virtual filesystems it is the filesystem name
          (e.g. ["proc"], ["sysfs"]). *)
  dst : string;
      (** Absolute path inside the container where the mount appears. *)
  options : string list;
      (** Mount options as space-less strings: ["ro"], ["rw"], ["nosuid"],
          ["rbind"], ["rprivate"], ["size=65536k"], etc. *)
}
(** One mount point. The field order mirrors the OCI runtime-spec layout for
    convenience but does not affect behaviour. *)

val to_json : t -> Yojson.Safe.t
(** [to_json m] serializes [m] to the JSON object expected by the OCI runtime
    spec:

    {[
      { "destination": dst;
        "type":        ty;
        "source":      src;
        "options":     [...] }
    ]}

    Normally called only by {!Oci_spec.make}; callers don't need to touch the
    JSON form themselves. *)

(** {2 Bind-mount helpers}

    Bind mounts make a host-side path visible inside the container at a
    different (or the same) location. They are the main way the build pipeline
    ships build inputs (opam-repository overlays, patches, pre-built dependency
    trees) into a container.

    Both helpers set [rbind] and [rprivate] so the mount is recursive and does
    not propagate back to the host mount namespace. *)

val bind_ro : src:string -> string -> t
(** [bind_ro ~src dst] is a read-only bind mount of host path [src] at container
    path [dst]. Use this for inputs the container should not be able to modify:
    source archives, patch files, the opam-repository, shared read-only dep
    trees, etc. *)

val bind_rw : src:string -> string -> t
(** [bind_rw ~src dst] is a read-write bind mount of host path [src] at
    container path [dst]. Use this sparingly — the container will be able to
    write to the host path directly. Typical uses are the shared odoc-output
    directories during doc generation. *)
