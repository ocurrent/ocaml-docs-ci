module Rpc = Current_rpc.Impl (Current)
module Raw = Pipeline_api.Raw

open Capnp_rpc_lwt

let make ~monitor:_ =
  let module Api = Raw.Service.Pipeline in

  let get_project _name =
    None
  in

  Api.local @@
    object
      inherit Api.service

      method projects_impl _params release_param_caps =
        let open Api.Projects in
        release_param_caps ();
        let response, results = Service.Response.create Results.init_pointer in
        (* TODO let blessings = Monitor.get_blessing monitor |> OpamPackage.Map.keys in *)
        let projects = [] in
        Results.projects_set_list results projects |> ignore;
        Service.return response

      method project_impl params release_param_caps =
        let open Api.Project in
        let project_name = Params.project_name_get params in
        release_param_caps ();
        match get_project project_name with
        | None -> Service.fail "Invalid project name %S" project_name
        | Some project ->
           let response, results = Service.Response.create Results.init_pointer in
           Results.project_set results (Some project);
           Service.return response
    end