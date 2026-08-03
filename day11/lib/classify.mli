(** Build failure classification.

    Scans build log content for known failure patterns to categorize failures as
    transient infrastructure issues, missing system dependencies, or genuine
    build failures. *)

val classify_build_log : string -> string * string * string option
(** [classify_build_log content] returns [(status, category, error_opt)] where:
    - [status] is ["failure"]
    - [category] is one of ["transient_failure"], ["depext_unavailable"], or
      ["build_failure"]
    - [error_opt] is an optional human-readable description *)

val extract_compiler_from_deps : Yojson.Safe.t -> string
(** [extract_compiler_from_deps json] extracts the compiler version from a
    layer.json's [deps] field. Looks for packages starting with
    [ocaml-base-compiler] or [ocaml-variants]. Returns [""] if no compiler is
    found. *)

val matches_any : string list -> string -> bool
(** [matches_any patterns text] returns [true] if any pattern in [patterns] is a
    case-insensitive substring of [text]. *)
