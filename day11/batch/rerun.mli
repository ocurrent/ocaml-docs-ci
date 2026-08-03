(** Rerun failed builds and cascade recovery.

    Layers are self-describing — uid, gid, and base_hash are read from
    layer.json. Opam files are read from the layer's own [opam-repository/]
    directory. No external configuration needed. *)

val rerun :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  cache_dir:Fpath.t ->
  Day11_opam_layer.Build.t ->
  Day11_opam_build.Types.build_result
(** [rerun ~sw env ~os_dir ~cache_dir node] rebuilds a failed layer. Reads
    uid/gid/base_hash from the layer's [layer.json] and opam files from its
    [opam-repository/] directory. *)

val cascade :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  cache_dir:Fpath.t ->
  Day11_opam_layer.Build.t list ->
  int
(** [cascade ~sw env ~os_dir ~cache_dir nodes] scans [nodes] for dependency
    failures where the dependency has since succeeded, and reruns them. Returns
    the number of packages rerun. *)
