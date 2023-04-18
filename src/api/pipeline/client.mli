open Capnp_rpc_lwt

module Build_status : sig
  type t = Raw.Reader.BuildStatus.t

  val pp : t Fmt.t
  val to_string : t -> string
end

module State : sig
  type t = Raw.Reader.JobInfo.State.unnamed_union_t

  val pp : t Fmt.t
  val from_build_status : [< `Failed | `Not_started | `Passed | `Pending ] -> t
end

module Project : sig
  type t = Raw.Client.Project.t Capability.t
  type project_version = { version : string }
  type project_status = { version : string; status : Build_status.t }

  val versions :
    t ->
    unit ->
    (project_version list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val status :
    Raw.Client.Project.t Capability.t ->
    unit ->
    (project_status list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module Pipeline : sig
  type t = Raw.Client.Pipeline.t Capability.t
  (** The top level object for ocaml-docs-ci. *)

  val project : t -> string -> Raw.Reader.Project.t Capability.t

  val projects :
    t ->
    ( Raw.Reader.ProjectInfo.t list,
      [> `Capnp of Capnp_rpc.Error.t ] )
    Lwt_result.t
end
