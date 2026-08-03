(** {!Xref_inter} exercises cross-package odoc xrefs. References to
    {!Basics.greeting} and {!Basics.length_of} should resolve to
    pages in the separately-built [basics] package. *)

(** Extended greeting using {!Basics.greeting}. *)
let extended = Basics.greeting ^ " (from xref_inter)"

(** Length of the extended greeting via {!Basics.length_of}. *)
let extended_length () = Basics.length_of extended
