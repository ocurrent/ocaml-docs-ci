open Capnp_rpc_lwt

module Build_status = struct
  include Raw.Reader.BuildStatus

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Failed -> Fmt.pf f "failed"
    | Passed -> Fmt.string f "passed"
    | Pending -> Fmt.string f "pending"
    | Undefined x -> Fmt.pf f "unknown:%d" x

  let to_string = function
    | NotStarted -> "not started"
    | Failed -> "failed"
    | Passed -> "passed"
    | Pending -> "pending"
    | Undefined _ -> "unknown"
end

module State = struct
  open Raw.Reader.JobInfo.State

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

module Project = struct
  type t = Raw.Client.Project.t Capability.t
  type project_version = { version : OpamPackage.Version.t }

  type project_status = {
    version : OpamPackage.Version.t;
    status : Build_status.t;
  }

  let versions t =
    let open Raw.Client.Project.Versions in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map (fun x ->
           x
           |> Results.versions_get_list
           |> List.map (fun x ->
                  {
                    version =
                      Raw.Reader.ProjectVersion.version_get x
                      |> OpamPackage.Version.of_string;
                  }))

  let status t =
    let open Raw.Client.Project.Status in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map (fun x ->
           x
           |> Results.status_get_list
           |> List.map (fun x ->
                  {
                    version =
                      Raw.Reader.ProjectBuildStatus.version_get x
                      |> OpamPackage.Version.of_string;
                    status = Raw.Reader.ProjectBuildStatus.status_get x;
                  }))
end

module Pipeline = struct
  type t = Raw.Client.Pipeline.t Capability.t

  let project t name =
    let open Raw.Client.Pipeline.Project in
    let request, params = Capability.Request.create Params.init_pointer in
    Params.project_name_set params name;
    Capability.call_for_caps t method_id request Results.project_get_pipelined

  let projects t =
    let open Raw.Client.Pipeline.Projects in
    let request = Capability.Request.create_no_args () in
    Capability.call_for_value t method_id request
    |> Lwt_result.map Results.projects_get_list
end
