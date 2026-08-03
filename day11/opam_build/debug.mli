(** Interactive debug containers for failed builds.

    Drops into an interactive shell inside a container with the failed package's
    dependencies stacked, source extracted, and opam tools available. *)

type session = {
  temp_dir : Fpath.t;
  os_dir : Fpath.t;
  build : Day11_opam_layer.Build.t;
  pkg : OpamPackage.t;
  uid : int;
  gid : int;
}

val setup :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  ?keep:bool ->
  Day11_opam_layer.Build.t ->
  (session, [> Rresult.R.msg ]) result
(** [setup ~sw env ~os_dir ?keep node] prepares a debug container for [node].
    Reads uid/gid/base from the layer's metadata. Derives [cache_dir] from
    [os_dir]. If [keep] is true, uses a stable directory name for re-entry. *)

val run_interactive : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> session -> int
(** [run_interactive ~sw env session] launches an interactive shell that
    attempts to build, then drops to bash on failure. Returns the exit code. *)

val run_command :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> session -> string -> int
(** [run_command ~sw env session cmd] runs [cmd] non-interactively in the debug
    container. Returns the exit code. *)

val teardown : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> session -> unit
(** [teardown ~sw env session] unmounts the overlay and cleans up. Call after
    the debug session unless [keep] was used. *)
