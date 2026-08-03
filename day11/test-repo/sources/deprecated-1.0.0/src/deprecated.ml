(** {!Deprecated} exercises odoc's rendering of deprecation
    markers. Both the [@deprecated] attribute and the
    [@deprecated] docstring tag should surface in the HTML. *)

(** New-world function — use this. *)
let modern_add (x : int) (y : int) = x + y

(** Old-world function.
    @deprecated Use {!modern_add} instead. *)
let legacy_add (x : int) (y : int) = x + y
  [@@deprecated "use modern_add"]
