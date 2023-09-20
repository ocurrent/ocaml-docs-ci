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
  type package_info = Raw.Reader.PackageInfo.t
  type package_info_list = package_info list [@@deriving to_yojson]

  type package_status = {
    version : OpamPackage.Version.t;
    status : Build_status.t;
  }

  val package_status_to_yojson : package_status -> Yojson.Safe.t

  type package_status_list = package_status list [@@deriving to_yojson]
  type step = { typ : string; job_id : string option; status : Build_status.t }

  type package_steps = {
    version : string;
    status : Build_status.t;
    steps : step list;
  }

  type package_steps_list = package_steps list [@@deriving to_yojson]

  val package_steps_to_yojson : package_steps -> Yojson.Safe.t
  val step_to_yojson : step -> Yojson.Safe.t

  val versions :
    t -> (package_status list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val steps :
    t ->
    ( (string * Build_status.t * step list) list,
      [> `Capnp of Capnp_rpc.Error.t ] )
    Lwt_result.t

  val by_pipeline :
    t ->
    int64 ->
    (package_status list, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end

module Pipeline : sig
  type t = Raw.Client.Pipeline.t Capability.t
  (** The top level object for ocaml-docs-ci. *)

  type health = Raw.Reader.PipelineHealth.t
  (** General information and health of the pipeline. Includes information about
      voodoo, the epochs, and the number of failing, running and passing
      packages *)

  val health_to_yojson :
    health -> [> `Assoc of (string * [> `Int of int | `String of string ]) list ]

  val package : t -> string -> Raw.Reader.Package.t Capability.t

  val packages :
    t ->
    ( Raw.Reader.PackageInfo.t list,
      [> `Capnp of Capnp_rpc.Error.t ] )
    Lwt_result.t

  val health :
    t ->
    int64 ->
    (Raw.Reader.PipelineHealth.t, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t

  val diff :
    t ->
    int64 ->
    int64 ->
    ( Raw.Reader.PackageInfo.t list,
      [> `Capnp of Capnp_rpc.Error.t ] )
    Lwt_result.t

  val pipeline_ids :
    t -> (int64 * int64, [> `Capnp of Capnp_rpc.Error.t ]) Lwt_result.t
end
