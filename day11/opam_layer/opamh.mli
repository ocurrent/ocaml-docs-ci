(** Opam switch state helpers. *)

val compiler_packages : OpamPackage.Name.t list
(** The set of packages that form the compiler stack. *)

val dump_state : Fpath.t list -> Fpath.t -> (unit, [> Rresult.R.msg ]) result
(** [dump_state packages_dirs state_file] reads the installed packages from all
    [packages_dirs], identifies compiler packages, and writes a switch-state
    file at [state_file]. *)
