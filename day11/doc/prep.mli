(** Prep structure creation for odoc.

    Creates the directory layout expected by [odoc_driver_voodoo] and returns
    bind mounts that map build layer lib directories directly into the prep
    structure. No file copying — the container sees files directly from cached
    layers. *)

val create_with_mounts :
  source_layer_dir:Fpath.t ->
  dest_layer_dir:Fpath.t ->
  universe:string ->
  pkg:OpamPackage.t ->
  installed_libs:string list ->
  installed_docs:string list ->
  (Fpath.t * Day11_container.Mount.t list, [> Rresult.R.msg ]) result
(** [create_with_mounts ~source_layer_dir ~dest_layer_dir ~universe ~pkg
     ~installed_libs ~installed_docs] creates the prep directory structure and
    returns [(prep_root, lib_mounts)]. The [lib_mounts] bind-mount each library
    directory from the build layer directly into the container's prep layout.
    Doc files (few, small) are copied directly. *)
