(** Basics — a minimal package used as the baseline for test-repo
    scenarios. Renders a docstring and a public value so that the
    rendered HTML has something to inspect. *)

(** [greeting] is a fixed welcome string.

    Used by downstream test packages as an entry point for cross-
    package xref tests. *)
let greeting = "hello, test lab"

(** [length_of s] is the byte length of [s]. *)
let length_of (s : string) : int = String.length s
