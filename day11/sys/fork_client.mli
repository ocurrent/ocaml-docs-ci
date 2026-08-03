(** Client for the fork helper daemon.

    Spawns subprocesses via a small helper process to avoid the ~100ms page
    table copy cost of forking the main 1.7GB process. *)

type t

val spawn :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  t ->
  env_arr:string array ->
  argv:string array ->
  output_file:string option ->
  string * string * [ `Exited of int | `Signaled of int ]
(** [spawn ~sw env t ~env_arr ~argv ~output_file] executes the command and
    returns [(stdout, stderr, status)]. The Unix-socket connection to the helper
    is bound to [sw]. The socket I/O is cooperative on the calling fiber's
    domain. *)

val get_instance : unit -> t
(** Return the global fork helper instance, starting it if needed. *)
