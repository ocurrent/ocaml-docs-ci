module Ssh : sig
  type t

  val secrets : Obuilder_spec.Secret.t list

  val secrets_values : t -> (string * string) list

  val host : t -> string

  val user : t -> string

  val priv_key_file : t -> Fpath.t

  val port : t -> int

  val storage_folder : t -> string

  val digest : t -> string
  (** Updated when the storage location changes *)
end

type t

val cmdliner : t Cmdliner.Term.t

val ssh : t -> Ssh.t

val odoc : t -> string
(** Odoc version pin to use. *)

val pool : t -> string
(** The ocluster pool to use *)

val ocluster_connection_prep : t -> Current_ocluster.Connection.t
(** Connection to the cluster for Prep *)

val ocluster_connection_do : t -> Current_ocluster.Connection.t
(** Connection to the cluster for Do *)

val ocluster_connection_gen : t -> Current_ocluster.Connection.t
(** Connection to the cluster for Gen *)

val jobs : t -> int
(** Number of jobs that can be spawned for the steps that are locally executed. *)

val track_packages : t -> string list
(** List of packages to track (or all packages if the list is empty) *)

val take_n_last_versions : t -> int option
(** Number of versions to take (None for all) *)
