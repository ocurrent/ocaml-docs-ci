(** Import layer filesystems from external sources. *)

val from_docker :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  image:string ->
  layer_dir:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [from_docker ~sw env ~image ~layer_dir] extracts the filesystem of a Docker
    image into [layer_dir/fs/]. Creates a temporary container via
    [docker create], exports it with [docker export], and extracts the tarball.
    The temporary container is removed afterward.

    The image is pulled if not already present locally. *)
