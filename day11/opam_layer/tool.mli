(** A "tool" is a doc-pipeline aggregate of one or more opam package layers
    (e.g. odoc, odoc-md, odoc_driver_voodoo, plus their deps) used as a fixed
    input to doc generation containers. *)

type t = { hash : string; dir : Fpath.t; builds : Build.t list }
