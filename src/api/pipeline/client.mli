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

module Package : sig
  type t = Raw.Client.Package.t Capability.t
  type package_version = { version : OpamPackage.Version.t }

  type package_status = {
    version : OpamPackage.Version.t;
    status : Build_status.t;
  }

  type step = {
    typ : string;
    job_id : string option;
    status : Build_status.t;
  }

  (* type package_steps = {
    status: Build_status.t;
    steps : step list;
  } *)

  val versions :
    t -> (package_status list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val steps :
    t ->
    (step list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module Pipeline : sig
  type t = Raw.Client.Pipeline.t Capability.t
  (** The top level object for ocaml-docs-ci. *)

  val package : t -> string -> Raw.Reader.Package.t Capability.t

  val packages :
    t ->
    ( Raw.Reader.PackageInfo.t list,
      [> `Capnp of Capnp_rpc.Error.t ] )
    Lwt_result.t
end
