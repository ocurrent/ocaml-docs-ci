(** Subprocess execution via Eio.

    Modelled on odoc_driver's [run.ml]. Launches subprocesses with proper
    stdout/stderr capture via Eio pipes. All day11 subprocess execution goes
    through this module. *)

type t = {
  cmd : string list;
  time : float;  (** Wall-clock time in seconds. *)
  output_file : Fpath.t option;
  output : string;  (** Captured stdout. *)
  errors : string;  (** Captured stderr. *)
  status : [ `Exited of int | `Signaled of int ];
}
(** Result of a subprocess execution. *)

val run :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Bos.Cmd.t -> Fpath.t option -> t
(** [run ~sw env cmd output_file] spawns [cmd] as a subprocess via the fork
    helper, captures stdout/stderr, and awaits completion. The socket connection
    to the helper is bound to [sw], so cancelling [sw] cancels the spawn.

    [output_file] is stored in the result for the caller's bookkeeping — it does
    not affect where output goes. *)
