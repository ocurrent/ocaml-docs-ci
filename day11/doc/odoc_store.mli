(** Package location and path utilities for odoc output.

    Used to compute relative paths for HTML output mounts and container mount
    points. *)

type pkg_loc = { pkg : OpamPackage.t; universe : string; blessed : bool }
(** Location info for a package. Determines the path prefix: blessed uses
    [p/<Name>/<version>/], non-blessed uses [u/<universe>/<Name>/<version>/]. *)

val rel_path : pkg_loc -> Fpath.t
(** The relative path fragment for a package location. *)

val container_html : string
(** Container path for HTML output: ["/home/opam/html"]. *)
