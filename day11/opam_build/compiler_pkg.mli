(** Identification of "real compiler" packages and a post-build sanity check on
    their install.

    The {e real} compiler is the package whose build script actually runs
    [./configure && make && make install] and lays down
    [lib/ocaml/stdlib*.cmti]. Mainline split this out of [ocaml-base-compiler]
    into [ocaml-compiler] at 5.3.0; oxcaml's equivalent is [oxcaml-compiler].
    Wrappers ([ocaml-variants], post-5.3.0 [ocaml-base-compiler],
    [ocaml-system]) carry [flags: compiler] in opam but install nothing of
    substance. *)

val names : OpamPackage.Name.t list
(** Cheap name-based prefilter: the set of names {!is_compiler} can return true
    for. Use {!is_compiler} for an authoritative check (it includes a version
    split). *)

val is_compiler : OpamPackage.t -> bool
(** [is_compiler pkg] returns true for the real-compiler packages:
    - [ocaml-compiler] {b ≥} 5.3.0
    - [ocaml-base-compiler] {b <} 5.3.0
    - [oxcaml-compiler] (any version). *)

val check_stdlib_installed :
  build_layer:Fpath.t -> OpamPackage.t -> (unit, [> Rresult.R.msg ]) result
(** [check_stdlib_installed ~build_layer pkg] is the post-build invariant for
    compiler packages: their build must have left [lib/ocaml/stdlib.cmti] in
    [build_layer/fs/home/opam/.opam/default/]. For non-compiler packages,
    returns [Ok ()] without checking. *)
