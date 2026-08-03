(** A layer on disk.

    A layer is identified by its content hash and lives at a directory derived
    from that hash under the os_dir. The directory contains:
    - [fs/] — the filesystem tree (overlayfs upper)
    - [layer.json] — metadata ({!Meta.t})
    - [layer.log] — build stdout/stderr
    - optional sidecar files ([build.json], [doc.json], etc.) *)

type t = { hash : string; dir : Fpath.t }

val of_hash : os_dir:Fpath.t -> string -> t
(** [of_hash ~os_dir hash] constructs a layer reference from a hash. Does not
    check whether the layer exists on disk. *)

val hash : t -> string
val dir : t -> Fpath.t

val fs : t -> Fpath.t
(** [fs t] is [t.dir / "fs"]. *)

val meta_path : t -> Fpath.t
(** [meta_path t] is [t.dir / "layer.json"]. *)

val log_path : t -> Fpath.t
(** [log_path t] is [t.dir / "layer.log"]. *)

val pp : Format.formatter -> t -> unit

val exists : Eio_unix.Stdenv.base -> t -> bool
(** [exists env t] returns true if [layer.json] exists on disk. *)

val is_ok : Eio_unix.Stdenv.base -> t -> bool
(** [is_ok env t] returns true if the layer exists and has exit_status = 0. *)
