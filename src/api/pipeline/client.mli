open Capnp_rpc_lwt

module Build_status : sig
  type t = Raw.Reader.BuildStatus.t

  val pp : t Fmt.t
  val color : t -> Fmt.style
end

module State : sig
  type t =
    | Aborted
    | Failed of string
    | NotStarted
    | Active
    | Passed
    | Undefined of int

  val pp : t Fmt.t
  val from_build_status : Build_status.t -> t
end

module Project : sig
  type t = Raw.Client.Project.t Capability.t
  type project_version = { version : OpamPackage.Version.t }

  type project_status = {
    version : OpamPackage.Version.t;
    status : Build_status.t;
  }

  val versions :
    t -> (project_version list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val status :
    Raw.Client.Project.t Capability.t ->
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
