open Capnp_rpc_lwt

module State = struct
  open Raw.Reader.JobInfo.State

  type t = unnamed_union_t

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Aborted -> Fmt.string f "aborted"
    | Failed m -> Fmt.pf f "failed: %s" m
    | Passed -> Fmt.string f "passed"
    | Active -> Fmt.string f "active"
    | Undefined x -> Fmt.pf f "unknown:%d" x

  let from_build_status = function
    | `Failed -> Failed ""
    | `Not_started -> NotStarted
    | `Pending -> Active
    | `Passed -> Passed
end

module Build_status = struct
  include Raw.Reader.BuildStatus

  let pp f = function
    | NotStarted -> Fmt.string f "not started"
    | Failed -> Fmt.pf f "failed"
    | Passed -> Fmt.string f "passed"
    | Pending -> Fmt.string f "pending"
    | Undefined x -> Fmt.pf f "unknown:%d" x
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