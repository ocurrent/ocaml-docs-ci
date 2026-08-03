(** Garbage collection for the build cache.

    Removes unreferenced build layers and stale odoc store entries. "Referenced"
    means the layer hash appears in the current DAG (computed from the latest
    solutions). *)

type result = { total : int; kept : int; deleted : int }

val gc_build_layers : os_dir:string -> referenced:string list -> result
(** [gc_build_layers ~os_dir ~referenced] deletes [build-*] directories in
    [os_dir] whose names are not in [referenced]. *)

val gc_odoc_store : os_dir:string -> referenced_universes:string list -> result
(** [gc_odoc_store ~os_dir ~referenced_universes] removes entries from
    [odoc-store/odoc-out/u/] and [odoc-store/html/u/] whose universe hashes are
    not in [referenced_universes]. Blessed ([p/]) entries are always kept. *)

val gc_stale_temp_dirs : unit -> int
(** Remove stale [day11_run_*] directories from the system temp dir. Returns the
    number deleted. *)
