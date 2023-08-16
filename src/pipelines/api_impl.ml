module Rpc = Current_rpc.Impl (Current)
module Raw = Pipeline_api.Raw
module Monitor = Docs_ci_lib.Monitor
module Index = Docs_ci_lib.Index
module String_map = Map.Make (String)
open Capnp_rpc_lwt

let make_package ~monitor package_name =
  let module Api = Raw.Service.Package in
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
             (OpamPackage.Name.of_string package_name)
             versions_map
         in
         match versions with
         | None -> Service.fail "Invalid package name %S" package_name
         | Some versions ->
             let arr = Results.versions_init results (List.length versions) in
             versions
             |> List.iteri (fun i (version, state) ->
                    let open Raw.Builder.PackageBuildStatus in
                    let slot = Capnp.Array.get arr i in
                    version_set slot (OpamPackage.Version.to_string version);
                    match state with
                    | Monitor.Done -> status_set slot Passed
                    | Running -> status_set slot Pending
                    | Failed -> status_set slot Failed);

             Service.return response

       method steps_impl _params release_param_caps =
         let open Api.Steps in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let all_package_versions_steps =
           Monitor.lookup_steps monitor ~name:package_name
           |> Result.value ~default:[] (* discard error and return [] *)
         in
         let arr =
           Results.steps_init results (List.length all_package_versions_steps)
         in
         let steps_f (step : Monitor.step) =
           let open Raw.Builder.StepInfo in
           let slot = init_root () in
           type_set slot step.typ;
           let job_id_t = job_id_init slot in
           (match step.job_id with
           | None -> JobId.none_set job_id_t
           | Some job_id -> (
               JobId.id_set job_id_t job_id;
               match step.status with
               | Active -> status_set slot Pending
               | Blocked -> status_set slot NotStarted
               | OK -> status_set slot Passed
               | Err _ -> status_set slot Failed));
           slot
         in
         all_package_versions_steps
         |> List.iteri (fun i (package_steps : Monitor.package_steps) ->
                let open Raw.Builder.PackageSteps in
                let slot = Capnp.Array.get arr i in
                version_set slot
                  (OpamPackage.version_to_string package_steps.package);
                (match package_steps.status with
                | Monitor.Done -> status_set slot Passed
                | Running -> status_set slot Pending
                | Failed -> status_set slot Failed);
                (match package_steps.status with
                | Monitor.Done -> status_set slot Passed
                | Running -> status_set slot Pending
                | Failed -> status_set slot Failed);

                let steps = package_steps.steps |> List.map steps_f in
                ignore (steps_set_list slot steps));

         Service.return response

       method by_pipeline_impl params release_param_caps =
         let open Api.ByPipeline in
         let pipeline_id = Params.pipeline_id_get params in
         release_param_caps ();

         let response, results = Service.Response.create Results.init_pointer in
         let versions =
           Index.get_package_status_by_name package_name pipeline_id
         in
         let arr = Results.versions_init results (List.length versions) in
         versions
         |> List.filter (fun (_, state_res) -> Result.is_ok state_res)
         |> List.iteri (fun i (version, state) ->
                let open Raw.Builder.PackageBuildStatus in
                let slot = Capnp.Array.get arr i in
                version_set slot version;
                match Result.get_ok state with
                | Monitor.Done -> status_set slot Passed
                | Running -> status_set slot Pending
                | Failed -> status_set slot Failed);
         Service.return response
     end

let make ~monitor =
  let module Api = Raw.Service.Pipeline in
  let packages = ref String_map.empty in
  let get_package name =
    match String_map.find_opt name !packages with
    | Some x -> Some x
    | None -> (
        let known_packages = Monitor.lookup_known_packages monitor in
        match List.find_opt (fun n -> n = name) known_packages with
        | None -> None
        | Some _ ->
            let package = make_package ~monitor name in
            packages := String_map.add name package !packages;
            Some package)
  in

  Api.local
  @@ object
       inherit Api.service

       method packages_impl _params release_param_caps =
         let open Api.Packages in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         let packages = Monitor.lookup_known_packages monitor in
         let arr = Results.packages_init results (List.length packages) in
         packages
         |> List.iteri (fun i package_name ->
                let open Raw.Builder.PackageInfo in
                let slot = Capnp.Array.get arr i in
                name_set slot package_name);
         Service.return response

       method package_impl params release_param_caps =
         let open Api.Package in
         let package_name = Params.package_name_get params in
         release_param_caps ();
         match get_package package_name with
         | None -> Service.fail "Invalid package name %S" package_name
         | Some package ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.package_set results (Some package);
             Service.return response

       method health_impl params release_param_caps =
         let open Api.Health in
         let pipeline_id = Params.pipeline_id_get params in
         release_param_caps ();
         let response, results = Service.Response.create Results.init_pointer in
         match Index.get_pipeline_data pipeline_id with
         | None ->
             Service.fail "Invalid pipeline_id %d" (Int64.to_int pipeline_id)
         | Some pipeline_data ->
             let Index.{ failed_count; running_count; passed_count } =
               Index.get_pipeline_counts pipeline_id
             in
             let health_slot = Results.health_init results in
             let open Raw.Builder.PipelineHealth in
             voodoo_do_commit_set health_slot pipeline_data.voodoo_do;
             voodoo_gen_commit_set health_slot pipeline_data.voodoo_gen;
             voodoo_prep_commit_set health_slot pipeline_data.voodoo_prep;
             epoch_html_set health_slot pipeline_data.epoch_html;
             epoch_linked_set health_slot pipeline_data.epoch_linked;
             failing_packages_set health_slot @@ Int64.of_int failed_count;
             passing_packages_set health_slot @@ Int64.of_int passed_count;
             running_packages_set health_slot @@ Int64.of_int running_count;

             Service.return response

       method diff_impl params release_param_caps =
         let open Api.Diff in
         let pipeline_id_one = Params.pipeline_id_one_get params in
         let pipeline_id_two = Params.pipeline_id_two_get params in
         release_param_caps ();

         let response, results = Service.Response.create Results.init_pointer in
         let failing_packages_that_were_passing =
           Index.get_pipeline_diff ~pipeline_id_latest:pipeline_id_one
             ~pipeline_id_latest_but_one:pipeline_id_two
         in
         let arr =
           Results.failing_packages_init results
             (List.length failing_packages_that_were_passing)
         in
         failing_packages_that_were_passing
         |> List.iteri (fun i (name, version) ->
                let open Raw.Builder.PackageInfo in
                let slot = Capnp.Array.get arr i in
                name_set slot (Fmt.str "%s:%s" name version));
         Service.return response
     end
