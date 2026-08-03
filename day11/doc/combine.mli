(** Local documentation aggregation via overlayfs.

    Mounts all successful doc layers as an overlay filesystem for local viewing.
    Requires sudo for mount/umount. *)

type doc_layer = {
  pkg : OpamPackage.t;
  layer_hash : string;
  prep_path : Fpath.t;
  universe : string;
  blessed : bool;
}
(** A doc layer discovered in the cache. *)

val scan_cache : Eio_unix.Stdenv.base -> os_dir:Fpath.t -> doc_layer list
(** [scan_cache env ~os_dir] scans [os_dir] for [doc-*] layer directories
    (excluding tool layers), parses their layer.json, and returns
    successfully-generated doc layers. *)

val mount_overlay :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  layers:doc_layer list ->
  mount_point:Fpath.t ->
  work_dir:Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [mount_overlay ~sw env ~layers ~mount_point ~work_dir] mounts all doc
    layers' prep directories as a combined overlay at [mount_point]. *)

val unmount :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [unmount ~sw env mount_point] unmounts the doc overlay. *)
