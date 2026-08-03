(** Doc tool layer management.

    Manages two types of tool layers:
    - {b Driver layer} (shared): [odoc_driver_voodoo], [sherlodoc], [odoc-md] —
      built once with a fixed OCaml version.
    - {b Odoc layer} (per OCaml version): [odoc] — must match the target
      compiler since [.cmt]/[.cmti] formats are version-specific.

    Hash and name functions are pure. Existence checks do I/O. *)

(** {2 Driver layer} *)

val driver_layer_hash :
  base_hash:string -> compiler_hashes:string list -> string
(** Hash for the shared driver layer. *)

val driver_layer_name :
  base_hash:string -> compiler_hashes:string list -> string
(** Directory name: [doc-driver-{hash}]. *)

val driver_build_script :
  packages:string list -> pin_commands:string list -> string
(** Generate the shell script to install driver tools. [packages] is the list of
    opam packages to install. [pin_commands] are optional [opam pin] commands
    for local repos. *)

val driver_exists : Eio_unix.Stdenv.base -> layer_dir:Fpath.t -> bool
(** Check if the driver layer has been built (layer.json exists). *)

val has_odoc_driver_voodoo : layer_dir:Fpath.t -> bool
(** Check if [odoc_driver_voodoo] binary exists in the layer. *)

(** {2 Odoc layer (per OCaml version)} *)

val odoc_layer_hash :
  base_hash:string ->
  ocaml_version:string ->
  compiler_hashes:string list ->
  string
(** Hash for the per-version odoc layer. *)

val odoc_layer_name :
  base_hash:string ->
  ocaml_version:string ->
  compiler_hashes:string list ->
  string
(** Directory name: [doc-odoc-{hash}]. *)

val odoc_build_script :
  packages:string list -> pin_commands:string list -> string
(** Generate the shell script to install odoc. *)

val odoc_exists : Eio_unix.Stdenv.base -> layer_dir:Fpath.t -> bool
(** Check if the odoc layer has been built. *)

val has_odoc : layer_dir:Fpath.t -> bool
(** Check if the [odoc] binary exists in the layer. *)
