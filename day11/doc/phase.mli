(** Documentation generation phases and results. *)

type doc_phase = Doc_all | Doc_compile_only | Doc_link_only

type doc_result =
  | Doc_success of { html_path : string; blessed : bool }
  | Doc_skipped
  | Doc_failure of string

val doc_result_to_yojson : doc_result -> Yojson.Safe.t
val doc_result_of_yojson : Yojson.Safe.t -> (doc_result, string) result
val phase_to_string : doc_phase -> string
