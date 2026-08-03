(** JTW generation logic.

    Pure functions for computing hashes, generating dynamic_cmis.json, findlib
    indexes, content hashes, and container scripts. *)

(** {2 Hashing} *)

val compute_layer_hash : build_hash:string -> tools_hash:string -> string
(** Hash for a JTW layer, depending on the build and tools layers. *)

val compute_content_hash : string -> string
(** [compute_content_hash lib_dir] hashes payload files (.cmi, .cma.js, META) in
    [lib_dir]. Returns first 16 hex chars. *)

val compute_compiler_content_hash : string -> string
(** [compute_compiler_content_hash tools_output_dir] hashes worker.js and stdlib
    .cmi files. Returns first 16 hex chars. *)

(** {2 JSON generation} *)

val generate_dynamic_cmis_json : dcs_url:string -> string list -> string
(** [generate_dynamic_cmis_json ~dcs_url cmi_filenames] generates the
    [dynamic_cmis.json] content. Partitions modules into toplevel (visible) and
    hidden ([__]-prefixed) with file prefixes. *)

val generate_findlib_index : compiler:Yojson.Safe.t -> string list -> string
(** [generate_findlib_index ~compiler meta_paths] generates the
    [findlib_index.json] content with compiler info and META paths. *)

(** {2 Findlib} *)

val findlib_names_of_installed_libs : string list -> string list
(** [findlib_names_of_installed_libs installed_libs] extracts top-level findlib
    package names from installed lib file paths by finding META files. Returns
    deduplicated, sorted names. *)

(** {2 Container scripts} *)

val container_script : pkg:OpamPackage.t -> installed_libs:string list -> string
(** Generate the shell script for per-package JTW generation inside a container.
    Returns ["true"] if no findlib packages found. *)

(** {2 Result type} *)

type jtw_result = Jtw_success | Jtw_failure of string | Jtw_skipped

val jtw_result_to_yojson : jtw_result -> Yojson.Safe.t
