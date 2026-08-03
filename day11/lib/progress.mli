(** Batch progress tracking.

    Immutable state updated during a batch run, written as [progress.json] for
    the web dashboard to poll. Each mutation returns a new {!type:t} value. *)

(** The current phase of a batch run. *)
type phase = Solving | Blessings | Building | Gc | Completed

type t
(** Opaque progress state. Use {!create} to initialise and the [set_*] /
    [incr_*] functions to update. *)

val create : run_id:string -> start_time:string -> targets:string list -> t
(** Create initial progress state in the {!Solving} phase. *)

val set_phase : t -> phase -> t
(** Set the current phase. *)

val set_solutions : t -> found:int -> failed:int -> t
(** Record how many solutions were found and how many failed. *)

val set_build_total : t -> int -> t
(** Set the total number of builds (and docs) expected. *)

val incr_build_completed : t -> t
(** Increment the count of completed builds by one. *)

val incr_doc_completed : t -> t
(** Increment the count of completed doc builds by one. *)

val set_completed : t -> build:int -> doc:int -> t
(** Set completed build and doc counts directly. *)

val to_json : t -> Yojson.Safe.t
(** Serialize progress state to JSON. *)

val write : run_dir:string -> t -> unit
(** Write [progress.json] atomically into [run_dir]. *)

val delete : run_dir:string -> unit
(** Delete [progress.json] from [run_dir], ignoring errors. *)

val phase_to_string : phase -> string
(** Convert a phase to its lowercase string representation. *)
