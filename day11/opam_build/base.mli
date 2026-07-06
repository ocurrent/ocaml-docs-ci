(** Base image management.

    Builds and caches the root filesystem layer that all package builds
    start from. The base contains the profile's OS image (e.g. debian
    or ubuntu) with opam, build tools, and pre-initialised opam
    repositories. Docker is used to produce the image, which is then
    imported as a layer for overlayfs use.

    The base layer lives under its os_dir ([<cache>/<os_dir_name>/base]),
    so different OS variants never share — and silently overwrite — one
    directory.

    When a [digest] is provided (e.g. [sha256:abc123...] from the
    profile), the base image is pulled by digest for reproducibility.
    The digest is saved alongside the base layer so future loads use
    the same hash. *)

val os_dir_name :
  os_distribution:string -> os_version:string -> arch:string -> string
(** The per-OS directory name under the cache root
    ([<os_distribution>-<os_version>-<arch>]). Single source of truth;
    {!Day11_batch.Profile.os_dir_name} delegates here. *)

val base_dir_of_os_dir : Fpath.t -> Fpath.t
(** The base layer directory for a given os_dir ([os_dir / "base"]). *)

val ensure :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  image:string ->
  (Day11_layer.Base.t, [> Rresult.R.msg ]) result
(** Ensure a base layer exists for the given Docker [image] tag under
    [os_dir], building and importing it if not already cached. *)

val build :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  cache_dir:Fpath.t ->
  os_distribution:string ->
  os_version:string ->
  arch:string ->
  uid:int ->
  gid:int ->
  ?digest:string ->
  unit ->
  (Day11_layer.Base.t, [> Rresult.R.msg ]) result
(** Build a base layer from scratch. When [digest] is provided
    (e.g. from {!Day11_batch.Profile.base_image_digest}), the
    Dockerfile uses [debian\@sha256:...] instead of [debian:bookworm]
    for a reproducible base. The digest is saved on disk so that
    {!load_cached} returns a base with the correct hash. *)

val build_opam_build :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  cache_dir:Fpath.t ->
  arch:string ->
  ?opam_build_repo:Fpath.t ->
  unit ->
  (Fpath.t, [> Rresult.R.msg ]) result
(** Build the [opam-build] binary in a Docker container and cache it. *)

val opam_build_mount :
  cache_dir:Fpath.t ->
  ?opam_build_repo:Fpath.t ->
  unit ->
  Day11_container.Mount.t option
(** Return a read-only bind mount for the cached [opam-build] binary
    associated with the given [opam_build_repo] (or the upstream
    master build when [None]). Profiles with different local
    checkouts get their own cached binaries; the mount shadows the
    binary (if any) baked into the base image. *)

val hash : image:string -> string
(** Compute a content hash for a Docker image identifier (tag or digest). *)

val build_hash :
  os_distribution:string -> os_version:string -> arch:string ->
  ?digest:string -> unit -> string
(** Compute the base hash. When [digest] is provided, it is used
    instead of the tag name, ensuring the hash changes when the
    upstream image is updated. *)

val load_cached :
  os_dir:Fpath.t ->
  os_distribution:string -> os_version:string ->
  Day11_layer.Base.t option
(** Load a previously cached base layer. Uses the stored digest
    (if any) for the hash, otherwise falls back to the tag name. *)
