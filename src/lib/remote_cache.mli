type t
(** It's the `cache/` folder, containing artifacts folder hashes for incrementality purposes. 
 Basically it's a way to tell the pipeline if something gets deleted on the storage server, or needs to be rebuilt. 
*)

val ssh_pool : unit Current.Pool.t

type cache_key = string 

type digest = string

type build_result = Ok of digest | Failed

type cache_entry = (digest * build_result) option

val digest : cache_entry -> string

val pp : cache_entry Fmt.t

val folder_digest_exn : cache_entry -> string

val get : t -> Fpath.t -> cache_entry
(** [get t path] retrieves the hash of [path], or returns None if it doesn't exist. *)

val v : Config.Ssh.t -> t Current.t
(** [v ssh] is an ocurrent component that synchronises the remote `cache/` folder with a local one. 
  It requires the [ssh] configuration. *)

val sync : job:Current.Job.t -> t -> unit Lwt.t
(** Synchronize the local folder *)

val cmd_write_key : cache_key -> Fpath.t list -> string
(** [cmd_write_key key paths] is the command to in order to write the cache key for the given [paths].*)

val cmd_compute_sha256 : Fpath.t list -> string
(** [cmd_compute_sha256 paths] is the command to run in order to compute the hashes of the given [paths], 
  and store the data in a file in the `cache/` folder. *)

val cmd_sync_folder : t -> string
(** [cmd_sync_folder] is the command to synchronise the digests folder with upstream. To run with ocluster, the command needs to be provided with Config.Ssh.secrets and network access*)