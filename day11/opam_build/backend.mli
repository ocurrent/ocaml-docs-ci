(** Build-backend abstraction.

    A {b backend} turns a {!Day11_opam_layer.Build.t} node plus its dep layers
    into a populated [fs/] directory. Two concrete backends exist:

    - {!Container_backend} — the original implementation: stacks dep layers as
      an overlayfs, runs the build command inside a runc container, captures the
      overlay upper dir. Needs sudo, produces root-owned layers.

    - {!Native_backend} (added in Phase 2) — hardlinks dep layers into a temp
      prefix, runs the build command directly on the host, captures the diff via
      {!Day11_layer.Snapshot}. No sudo, produces user-owned layers.

    Shared layer format: both backends produce byte-identical [fs/] trees
    (modulo build-prefix paths that relocatable OCaml
    + [OPAM_SWITCH_PREFIX] resolve at runtime).

    The {!S.build} signature is a bit wider than a pure shared interface — it
    carries container-only options ([mounts], [prep_upper]) for backward
    compatibility with the rich API {!Build_layer.build} has always exposed.
    {!Native_backend} ignores those parameters. *)

module type S = sig
  val build :
    sw:Eio.Switch.t ->
    Eio_unix.Stdenv.base ->
    Types.build_env ->
    opam_repositories:Fpath.t list ->
    ?mounts:Day11_container.Mount.t list ->
    ?patches:Patches.t ->
    ?build_dirs:Fpath.t list ->
    ?prep_upper:(upper:Fpath.t -> lowers:Fpath.t list -> unit) ->
    ?strategy:Types.build_strategy ->
    Day11_opam_layer.Build.t ->
    target_fs:Fpath.t ->
    unit ->
    (Day11_sys.Run.t * Day11_layer.Meta.timing, [> Rresult.R.msg ]) result
  (** [build ~sw env benv ... node ~target_fs ()] runs the build for [node] and
      places the captured filesystem tree at [target_fs].

      The backend is responsible for:
      - stacking the dep layers (via overlayfs, hardlinks, or whatever mechanism
        fits)
      - running the build command
      - applying [strategy.cleanup] to the captured tree
      - {b moving / placing the captured files at [target_fs]} — [target_fs]
        must not exist beforehand; the backend creates it

      [opam_repositories] are the repo source paths a per-package slice is
      extracted from (just [node.pkg]) and mounted as the container's [default]
      repo, so opam only sees the one package rather than the whole
      opam-repository baked into the base image. It is {b required} (not
      optional) on purpose: a build only ever needs its own package, so omitting
      it used to silently fall back to the full base repo — a several-second
      switch-state load per node. Pass [[]] to deliberately opt into that
      fallback.

      Returns [(run, timing)] — [run.status] is the build's exit code; [timing]
      is a per-phase timing alist for [layer.json]'s [timing] field.

      Container-specific parameters ([mounts], [prep_upper]) are ignored by
      non-container backends; the caller is responsible for not relying on them
      when running under a backend that doesn't honour them. *)
end
