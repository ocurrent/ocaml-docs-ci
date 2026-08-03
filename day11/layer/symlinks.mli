(** Per-id tracking symlinks under [packages_dir/<id>/].

    A small registry that lets you enumerate every layer ever associated with a
    given identifier (typically a package [name.version] string, but the
    function is generic — any string that's a valid path component will do).

    For each layer, a symlink is created at [packages_dir/<id>/<layer_name>]
    pointing back to the layer directory. The directory name [packages] is
    historical from when every layer was an opam package build; this module just
    owns the symlink mechanics and does not interpret the namespace. *)

val ensure :
  Eio_unix.Stdenv.base ->
  packages_dir:Fpath.t ->
  id:string ->
  layer_name:string ->
  (unit, [> Rresult.R.msg ]) result
(** [ensure env ~packages_dir ~id ~layer_name] creates a symlink at
    [packages_dir/id/layer_name] pointing to [../../layer_name]. Creates the
    [packages_dir/id] directory if needed. Idempotent — if a symlink already
    exists at the same path it is replaced so the registry always reflects the
    most recent build. *)
