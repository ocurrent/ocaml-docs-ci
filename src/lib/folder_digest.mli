type t = unit (* todo: hide this type *)

val get : t -> Fpath.t -> string option

val v : unit -> t Current.t

val sync : job:Current.Job.t -> unit -> unit Lwt.t

val compute_cmd : Fpath.t list -> string
