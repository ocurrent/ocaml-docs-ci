(** Privileged execution.

    Container builds produce root-owned files. We need sudo for cleanup and
    overlay mounting. *)

val run :
  ?output_file:Fpath.t ->
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Bos.Cmd.t ->
  (Run.t, [> Rresult.R.msg ]) result
(** [run ?output_file ~sw env cmd] executes [cmd] with sudo prepended.
    [output_file] is stored on the resulting {!Run.t} for the caller's
    bookkeeping — it does not redirect output. *)

val rm_rf :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (unit, [> Rresult.R.msg ]) result
(** [rm_rf ~sw env path] removes [path] recursively via [sudo rm -rf]. *)
