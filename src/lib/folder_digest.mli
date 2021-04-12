type t
(** It's the `digests/` folder, containing artifacts folder hashes for incrementality purposes. 
 Basically it's a way to tell the pipeline if something gets deleted on the storage server, or needs to be rebuilt. 
*)

val get : t -> Fpath.t -> string option
(** [get t path] retrieves the hash of [path], or returns None if it doesn't exist. *)

val v : unit -> t Current.t
(** [v ()] is an ocurrent component that synchronises the remote `digests/` folder with a local one. *)

val sync : job:Current.Job.t -> unit -> unit Lwt.t
(** Synchronize the local folder *)

val compute_cmd : Fpath.t list -> string
(** [compute_cmd paths] is the command to run in order to compute the hashes of the given [paths] *)
