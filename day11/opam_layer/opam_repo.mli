(** Opam repository assembly.

    Assembles the [opam-repository/] subdirectory inside a layer or temp
    directory. This is pure filesystem work — copies opam files from source
    repositories. *)

val create : Fpath.t -> (Fpath.t, [> Rresult.R.msg ]) result
(** [create parent_dir] creates an [opam-repository/] subdirectory under
    [parent_dir] with a [repo] file. Returns the path to the created directory.
*)

val build_merged :
  dest:Fpath.t -> string list -> (unit, [> Rresult.R.msg ]) result
(** [build_merged ~dest opam_repositories] assembles a merged opam repository at
    [dest] by overlaying entries from each source repo, later ones overriding
    earlier ones at the package level. The first entry is copied wholesale (for
    top-level files like [repo] and [version]); subsequent entries contribute
    only their [packages/] trees. A no-op if [dest] already exists. *)

val populate :
  opam_repo:Fpath.t ->
  opam_repositories:Fpath.t list ->
  OpamPackage.t list ->
  (unit, [> Rresult.R.msg ]) result
(** [populate ~opam_repo ~opam_repositories packages] copies the opam file (and
    any [files/] directory) for each package in [packages] from the first
    matching directory in [opam_repositories] into [opam_repo]. Packages not
    found in any repository are silently skipped. *)

val snapshot_to_layer :
  layer_dir:Fpath.t ->
  opam_repositories:Fpath.t list ->
  ?patches:Fpath.t list ->
  OpamPackage.t ->
  (unit, [> Rresult.R.msg ]) result
(** [snapshot_to_layer ~layer_dir ~opam_repositories ?patches pkg] writes a
    single-package opam-repository slice into [<layer_dir>/opam-repository/],
    containing just [pkg]'s [opam] file and [files/] directory.

    If [?patches] is supplied, each patch file is copied into the slice's
    [files/] directory under a stable [NNN-<basename>] name and added to the
    opam file's [patches:] field. The result is a self-contained slice that can
    be mounted at [/home/opam/.opam/repo/default] for a deterministic
    re-install, with patches applied natively by opam (no [--patch] flag
    needed).

    Returns [Error] if [pkg] isn't present in any of [opam_repositories]. *)
