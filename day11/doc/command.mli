(** Shell command generation for odoc_driver_voodoo.

    Generates the command string to run inside a container for documentation
    generation. Pure — no I/O. *)

val odoc_driver_voodoo :
  pkg:OpamPackage.t ->
  universe:string ->
  blessed:bool ->
  actions:string ->
  odoc_bin:string ->
  odoc_md_bin:string ->
  string
(** [odoc_driver_voodoo ~pkg ~universe ~blessed ~actions ~odoc_bin ~odoc_md_bin]
    generates the shell command to run [odoc_driver_voodoo].

    [actions] is one of ["all"], ["compile-only"], or ["link-and-gen"].
    [odoc_bin] and [odoc_md_bin] are paths to the odoc and odoc-md binaries
    inside the container. *)

val compute_universe_hash : string list -> string
(** [compute_universe_hash dep_hashes] computes a deterministic hash from a
    sorted list of dependency doc hashes. Used to identify which universe a doc
    build belongs to. *)
