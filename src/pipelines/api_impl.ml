module Rpc = Current_rpc.Impl (Current)
module Raw = Pipeline_api.Raw
module Monitor = Docs_ci_lib.Monitor
module String_map = Map.Make (String)
open Capnp_rpc_lwt

let make_project ~monitor project_name =
  let module Api = Raw.Service.Project in
  Api.local
  @@ object
       inherit Api.service

       method versions_impl _params release_param_caps =
         let open Api.Versions in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let versions_map = Monitor.map_versions monitor in
         let versions =
           OpamPackage.Name.Map.find_opt
             (OpamPackage.Name.of_string project_name)
             versions_map
         in
         match versions with
         | None -> Service.fail "Invalid project name %S" project_name
         | Some versions ->
             let arr = Results.versions_init results (List.length versions) in
             versions
             |> List.iteri (fun i (version, _) ->
                    let open Raw.Builder.ProjectVersion in
                    let slot = Capnp.Array.get arr i in
                    version_set slot (OpamPackage.Version.to_string version));
             Service.return response

       method status_impl _params release_param_caps =
         let open Api.Status in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let statuses = Monitor.lookup_status monitor ~name:project_name in
         let arr = Results.status_init results (List.length statuses) in
         statuses
         |> List.iteri (fun i (_name, version, state) ->
                let open Raw.Builder.ProjectBuildStatus in
                let slot = Capnp.Array.get arr i in
                version_set slot (OpamPackage.Version.to_string version);
                match state with
                | Monitor.Done -> status_set slot Passed
                | Monitor.Failed -> status_set slot Failed
                | Monitor.Running -> status_set slot Pending);
         Service.return response
     end

let make ~monitor =
  let module Api = Raw.Service.Pipeline in
  let projects = ref String_map.empty in
  let get_project name =
    match String_map.find_opt name !projects with
    | Some x -> Some x
    | None -> (
        let known_projects = Monitor.lookup_known_projects monitor in
        match List.find_opt (fun n -> n = name) known_projects with
        | None -> None
        | Some _ ->
            let project = make_project ~monitor name in
            projects := String_map.add name project !projects;
            Some project)
  in

  Api.local
  @@ object
       inherit Api.service

       method projects_impl _params release_param_caps =
         let open Api.Projects in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let projects = Monitor.lookup_known_projects monitor in
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

       (* method status_impl params release_param_caps =
          let open Api.Status in
          let name = Params.project_name_get params in
          let version = Params.version_get params in
          release_param_caps ();
          let response, results = Service.Response.create Results.init_pointer in
          let slot = Results.status_init results in
          (match Monitor.lookup_status monitor ~name ~version with
          | None ->
             Raw.Builder.ProjectBuildStatus.status_set slot NotStarted; (* TODO: This is wrong *)
          | Some Done -> Raw.Builder.ProjectBuildStatus.status_set slot Passed
          | Some Running -> Raw.Builder.ProjectBuildStatus.status_set slot Pending
          | Some Failed -> Raw.Builder.ProjectBuildStatus.status_set slot Failed);
          Service.return response *)
     end
