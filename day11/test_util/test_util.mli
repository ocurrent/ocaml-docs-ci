(** Shared test helpers for day11 test suites. *)

val with_tmp_dir : (Fpath.t -> 'a) -> 'a
(** [with_tmp_dir f] creates a temporary directory, calls [f dir], then removes
    the directory. *)

val with_eio : (sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> 'a) -> 'a
(** [with_eio f] runs [f] inside an Eio event loop, opening a root switch to
    which long-running resources can be attached. *)

val ok_or_fail : string -> ('a, [< `Msg of string ]) result -> 'a
(** [ok_or_fail msg r] extracts [Ok v] or calls [Alcotest.fail]. *)

val mkdir : Fpath.t -> unit
(** [mkdir path] creates a directory (with parents). *)

val write_file : Fpath.t -> string -> unit
(** [write_file path contents] writes a file. *)

val is_integration : unit -> bool
(** Returns [true] if [DAY11_INTEGRATION=true] is set. *)

val sudo_available : unit -> bool
(** Returns [true] if [sudo -n true] succeeds — i.e. the test process can run
    sudo without prompting for a password. Use to gate tests that require
    privileged execution. *)

val opam_repository : unit -> string
(** Returns the path to the opam-repository from [OPAM_REPOSITORY] environment
    variable. Calls [Alcotest.skip] if not set. *)
