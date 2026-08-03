(** Opam variable environment for dependency filtering.

    Provides the environment function that the solver context uses to evaluate
    opam filter expressions. Pure — no I/O. *)

val std_env :
  ?ocaml_native:bool ->
  ?sys_ocaml_version:string ->
  ?opam_version:string ->
  arch:string ->
  os:string ->
  os_distribution:string ->
  os_family:string ->
  os_version:string ->
  unit ->
  string ->
  OpamVariable.variable_contents option
(** [std_env ~arch ~os ~os_distribution ~os_family ~os_version ()] returns an
    environment function for system-level opam variables.

    Handles: [arch], [os], [os-distribution], [os-version], [os-family],
    [opam-version], [sys-ocaml-version], [ocaml:native]. *)
