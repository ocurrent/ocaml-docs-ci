(** {!Stdlib_ref} exercises Stdlib xref resolution. If the
    compiler's own odoc output is visible during link, references
    to {!Stdlib.Format.formatter} and {!Stdlib.stdout} resolve to
    live links into the stdlib page; otherwise they render as
    [xref-unresolved]. *)

(** The standard formatter, of type {!Stdlib.Format.formatter}. *)
let fmt : Stdlib.Format.formatter = Stdlib.Format.std_formatter

(** The standard {!Stdlib.out_channel} for stdout. *)
let chan : out_channel = Stdlib.stdout
