(** odoc doc-pipeline sidecar: per-layer metadata for documentation layers
    (compile / link / doc-all phases of the odoc driver).

    Written next to {!Day11_layer.Meta} as [doc.json] in the layer directory.
    The presence of this file marks a layer as odoc output, as opposed to (e.g.)
    an opam package build layer marked by [build.json] from [day11_opam_layer].
    This library is the minimal "doc layer" data layer — it depends only on
    [day11_layer], not on any opam-format types.

    The phase distinguishes the three odoc-driver invocation modes:
    - {!Compile} produces [.odoc] files (compile-only)
    - {!Link} produces [.odocl] linked output
    - {!Doc_all} runs the whole pipeline (compile + link + html) in one
      container, the common case for packages that don't need a separate link
      phase. *)

type phase = Compile | Link | Doc_all

val string_of_phase : phase -> string
val phase_of_string : string -> phase

type t = { package : string; phase : phase; deps : string list }

val filename : string
(** ["doc.json"] *)

val save : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
val load : Fpath.t -> (t, [> Rresult.R.msg ]) result
val exists : Fpath.t -> bool
