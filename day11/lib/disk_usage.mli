(** Disk usage reporting by category. *)

type report = {
  base : int;
  builds : int;
  docs : int;
  jtw : int;
  solutions : int;
  logs : int;
  packages : int;
  total : int;
}

val scan : os_dir:Fpath.t -> cache_dir:Fpath.t -> report
(** [scan ~os_dir ~cache_dir] computes disk usage in bytes for
    each category. *)

val layer_meta_total : cache_dir:Fpath.t -> int
(** [layer_meta_total ~cache_dir] sums the recorded [disk_usage] metadata
    of every build layer across all os_dirs under [cache_dir] — from each
    layer's [layer.json], not by measuring the tree. Reads one small JSON
    per layer (many at scale); run it off the event loop. *)

val pp : report Fmt.t
(** Pretty-print a report with human-readable sizes. *)
