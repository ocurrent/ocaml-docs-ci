(** Broken — intentionally uncompilable canary for the failure-
    display path in the GUI. The type annotation below does not
    match the expression, so [ocamlc] emits a one-line type error
    that shows up in the snapshot's history.Error column. *)

let x : int = "not an int"
