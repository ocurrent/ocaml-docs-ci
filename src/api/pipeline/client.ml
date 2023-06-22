open Capnp_rpc_lwt

module Build_status = struct
  include Raw.Reader.BuildStatus

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Failed -> Fmt.pf f "@{<red>failed@}"
    | Passed -> Fmt.pf f "@{<green>passed@}"
    | Pending -> Fmt.pf f "@{<yellow>pending@}"
    | Undefined x -> Fmt.pf f "unknown:%d" x

  let color = function
    | NotStarted -> `None
    | Failed -> `Fg `Red
    | Passed -> `Fg `Green
    | Pending -> `Fg `Yellow
    | Undefined _ -> `None

  let to_yojson = function
    | NotStarted -> `String "not started"
    | Failed -> `String "failed"
    | Passed -> `String "passed"
    | Pending -> `String "pending"
    | Undefined _ -> `String "unknown"
end

module State = struct
  type t =
    | Aborted
    | Failed of string
    | NotStarted
    | Active
    | Passed
    | Undefined of int

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Aborted -> Fmt.string f "aborted"
    | Failed m -> Fmt.pf f "failed: %s" m
    | Passed -> Fmt.string f "passed"
    | Active -> Fmt.string f "active"
    | Undefined x -> Fmt.pf f "unknown:%d" x

  let from_build_status = function
    | Build_status.Failed -> Failed ""
    | NotStarted -> NotStarted
    | Pending -> Active
    | Passed -> Passed
    | Undefined x -> Undefined x
end

module Package = struct
  type t = Raw.Client.Package.t Capability.t
  type package_version = { version : OpamPackage.Version.t }

  type package_status = {
    version : OpamPackage.Version.t;
    status : Build_status.t;
  }

  type step = { typ : string; job_id : string option; status : Build_status.t }
  [@@deriving to_yojson]

  let versions t =
    let open Raw.Client.Package.Versions in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map (fun x ->
           x
           |> Results.versions_get_list
           |> List.map (fun x ->
                  {
                    version =
                      Raw.Reader.PackageBuildStatus.version_get x
                      |> OpamPackage.Version.of_string;
                    status = Raw.Reader.PackageBuildStatus.status_get x;
                  }))

  let steps t version =
    let open Raw.Client.Package.Steps in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.package_version_set params version;
    Capability.call_for_value t method_id request
    |> Lwt_result.map (fun x ->
           x
           |> Results.steps_get_list
           |> List.map (fun x ->
                  let status = Raw.Reader.StepInfo.status_get x in
                  let typ = Raw.Reader.StepInfo.type_get x in
                  let job_id_t = Raw.Reader.StepInfo.job_id_get x in
                  let job_id =
                    match Raw.Reader.StepInfo.JobId.get job_id_t with
                    | Raw.Reader.StepInfo.JobId.None
                    | Raw.Reader.StepInfo.JobId.Undefined _ ->
                        None
                    | Raw.Reader.StepInfo.JobId.Id s -> Some s
                  in
                  { typ; status; job_id }))
end

module Pipeline = struct
  type t = Raw.Client.Pipeline.t Capability.t

  let package t name =
    let open Raw.Client.Pipeline.Package in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.package_name_set params name;
    Capability.call_for_caps t method_id request Results.package_get_pipelined

  let packages t =
    let open Raw.Client.Pipeline.Packages in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.packages_get_list
end
