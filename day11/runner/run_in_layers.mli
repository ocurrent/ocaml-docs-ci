(** Run a command in a container with layers stacked.

    Handles the generic container lifecycle: stack a base layer + dep layers as
    an overlayfs, optionally seed the upper with domain-specific files via
    [~prep_upper], mount, run the container described by an
    {!Day11_container.Oci_spec.t}, clean up.

    This module knows nothing about opam, opam switches, or doc generation. Any
    domain-specific concerns (writing an opam switch-state file, chowning home
    directories, mkdir'ing mount points for the container's bind mounts,
    choosing the cwd / hostname / env / argv that the contained process expects)
    are the caller's responsibility — supplied via [prep_upper] for upper-dir
    prep, and via the [Oci_spec.t] for everything else. *)

val run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  base:Day11_layer.Base.t ->
  build_dirs:Fpath.t list ->
  ?prep_upper:(upper:Fpath.t -> lowers:Fpath.t list -> unit) ->
  Day11_container.Oci_spec.t ->
  (Day11_sys.Run.t * Fpath.t * (string * float) list, [> Rresult.R.msg ]) result
(** [run ~sw env ~base ~build_dirs ?prep_upper spec] mounts an overlayfs rootfs
    from [base] + [build_dirs], optionally seeded by [prep_upper], then runs the
    container described by [spec].

    {b The lifecycle:}
    + Make a temp dir with upper/work/merged/lower subdirs.
    + Touch every dep layer (LRU bookkeeping via {!Day11_layer.Last_used}).
    + Plan the lowerdir layout via {!Day11_layer.Stack.plan_lowerdir},
      cp-merging excess layers if the mount-options string would overflow
      PAGE_SIZE.
    + Call [prep_upper ~upper ~lowers] (if supplied) so the caller can seed the
      upper with whatever files / chowns / mkdirs the container will need.
      [lowers] is the final list of lowerdirs in their mount order (separate dep
      dirs first, then merged lower if any). The caller can read from these to
      populate the upper based on dep contents.
    + Mount overlayfs at [merged].
    + Instantiate [spec] with [merged] as the rootfs path and write
      [config.json] into the bundle dir.
    + Run runc.
    + Umount and clean up everything except [upper], which the caller takes
      ownership of.

    [spec] is a fully-described container template — every field except the
    rootfs path is baked in. {!Day11_container.Oci_spec} documents the defaults;
    the caller is responsible for choosing cwd, env, hostname, network, mounts,
    and argv.

    Returns [(run_result, upper_dir, timing)] on success. [timing] is an alist
    of [(phase_name, seconds)] pairs in the order each phase ran (merge,
    prep_upper, overlay_mount, runc_run, overlay_umount, cleanup, total). The
    caller is responsible for extracting what they need from [upper_dir] and
    cleaning it up. *)
