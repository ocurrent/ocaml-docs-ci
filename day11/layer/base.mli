(** The base image layer.

    A {!t} represents the foundational layer at the bottom of every overlay
    stack — typically a Debian rootfs imported from a Docker image, with opam
    preinstalled. Higher-level layer types (opam package builds, odoc doc
    layers) live in domain-specific libraries, e.g. [day11_opam_layer]. *)

type t = { hash : string; dir : Fpath.t; image : string }
