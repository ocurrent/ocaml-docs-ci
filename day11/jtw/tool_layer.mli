(** JTW tool layer management.

    Per OCaml version: installs [js_of_ocaml] and [js_top_worker], builds
    [worker.js], extracts stdlib CMIs.

    Hash and name functions are pure. Existence checks do I/O. *)

val jtw_packages : string list
(** The opam packages that form the JTW toolchain. *)

val layer_hash :
  base_hash:string ->
  ocaml_version:string ->
  compiler_hashes:string list ->
  string

val layer_name :
  base_hash:string ->
  ocaml_version:string ->
  compiler_hashes:string list ->
  string
(** Directory name: [jtw-tools-{hash}]. *)

val build_script :
  packages:string list ->
  pin_commands:string list ->
  needs_compiler:bool ->
  compiler_pkg:string ->
  string
(** Generate the shell script to install JTW tools and build worker.js + stdlib
    artifacts. *)

type pin = { package : string; url : string }

val build_cmd : repo:string -> branch:string -> extra_pins:pin list -> string
(** [build_cmd ~repo ~branch ~extra_pins] returns the shell command that pins
    js_top_worker packages from [repo#branch], pins [extra_pins], installs
    js_of_ocaml and js_top_worker, and builds worker.js + stdlib artifacts. *)

val build_cmd_local : container_path:string -> extra_pins:pin list -> string
(** [build_cmd_local ~container_path ~extra_pins] returns the shell command that
    pins js_top_worker packages from a local path (bind-mounted into the
    container), pins [extra_pins], installs and builds worker.js. The caller
    must provide the bind mount. *)

val tool_target : string
(** The primary opam package name for the jtw tool binary. *)

val extra_tool_targets : string list
(** Additional packages needed in the jtw tool layer beyond [tool_target] (e.g.
    [js_top_worker-web] for worker.js). *)

val exists : layer_dir:Fpath.t -> bool
(** Check if the tool layer has been built. *)

val has_jsoo : layer_dir:Fpath.t -> bool
(** Check if [js_of_ocaml] binary exists in the layer. *)

val has_worker_js : layer_dir:Fpath.t -> bool
(** Check if [worker.js] was built in the layer. *)
