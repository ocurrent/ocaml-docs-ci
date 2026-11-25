type t

val cmdliner : t Cmdliner.Term.t
val jobs : t -> int
val track_packages : t -> string list
val take_n_last_versions : t -> int option

val doc_modules_max_depth : t -> int
(** [--doc-modules-max-depth]: see {!Day11_doc.Doc_modules.modules_of_sidebar}.
*)

val doc_modules_max_paths : t -> int
(** [--doc-modules-max-paths]: see {!Day11_doc.Doc_modules.modules_of_sidebar}.
*)
