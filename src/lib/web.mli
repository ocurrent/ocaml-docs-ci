type t

val make : unit -> t
(** Create a Web API instance *)

module Status : sig
  type bless_status = Blessed | Universe 

  type pending_status =  Prep | Compile of bless_status

  type t =
    | Pending of pending_status
    | Failed
    | Success of bless_status

  val to_int : t -> int

  val compare : t -> t -> int

  val pp : t Fmt.t
end


val set_package_status : package:Package.t Current.t -> status:Status.t Current.t -> t -> unit Current.t

val serve : port:int -> t -> unit Lwt.t
(** Serve the API *)