module Rpc = Current_rpc.Impl (Current)
module Raw = Pipeline_api.Raw
module Monitor = Docs_ci_lib.Monitor
open Capnp_rpc_lwt

let make ~monitor =
  let module Api = Raw.Service.Pipeline in
  let get_project _name = None in

  Api.local
  @@ object
       inherit Api.service

       method projects_impl _params release_param_caps =
         let open Api.Projects in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let blessings = Monitor.get_blessing monitor |> OpamPackage.Map.keys in
         let projects =
           List.map
             (fun blessing -> OpamPackage.name_to_string blessing)
             blessings
         in
         let arr = Results.projects_init results (List.length projects) in
         projects
         |> List.iteri (fun i project_name ->
                let open Raw.Builder.ProjectInfo in
                let slot = Capnp.Array.get arr i in
                name_set slot project_name);
         Service.return response

       method project_impl params release_param_caps =
         let open Api.Project in
         let project_name = Params.project_name_get params in
         release_param_caps ();
         match get_project project_name with
         | None -> Service.fail "Invalid project name %S" project_name
         | Some project ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.project_set results (Some project);
             Service.return response

       method status_impl params release_param_caps =
         let open Api.Status in
         let project_name = Params.project_name_get params in
         let _version = Params.version_get params in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let slot = Results.status_init results in
         let status = Monitor.lookup_solve_failures monitor project_name in
         (match status with
         | None -> Raw.Builder.ProjectBuildStatus.status_set slot Passed
         | Some _ -> Raw.Builder.ProjectBuildStatus.status_set slot Failed);
         Service.return response
     end
