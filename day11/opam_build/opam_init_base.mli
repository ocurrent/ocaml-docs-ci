(** opam-init seed layer for the native backend.

    Native builds need an [OPAMROOT] with at least one switch before
    [opam-build] will agree to run. This module produces such a root once and
    caches it as a {!Day11_layer.Layer.t}, so every native build can stack it as
    an implicit bottom dep.

    The layer contains [fs/home/opam/.opam/] populated by running
    [opam init --bare --no-setup --disable-sandboxing] and
    [opam switch create default --empty]. The resulting tree matches the path
    convention day11 container layers use, so the same [Stack.merge] +
    [.opam-switch/packages]-scanning code applies. *)

val ensure :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  ?opam_repositories:Fpath.t list ->
  unit ->
  (Day11_layer.Layer.t, [> Rresult.R.msg ]) result
(** [ensure ~sw env ~os_dir ?opam_repositories ()] returns the opam-init layer,
    building it if not already cached. The layer's hash is derived
    deterministically from the [opam --version], the init flags, and the list of
    opam repositories (by name — not commit SHA, so repo-content changes do not
    invalidate the init layer). *)
