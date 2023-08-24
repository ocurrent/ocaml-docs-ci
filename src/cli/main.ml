open Lwt.Infix
open Capnp_rpc_lwt
module Client = Pipeline_api.Client

let errorf msg = msg |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let with_ref r fn =
  Lwt.finalize
    (fun () -> fn r)
    (fun () ->
      Capability.dec_ref r;
      Lwt.return_unit)

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> errorf "$HOME not set! Can't get default cap file location.@."
      | Some home ->
          let path = Filename.concat home ".ocaml-ci.cap" in
          if Sys.file_exists path then Capnp_rpc_unix.Cap_file.load vat path
          else errorf "Default cap file %S not found!" path)

let pp_package_info f (pi : Pipeline_api.Raw.Reader.PackageInfo.t) =
  Fmt.pf f "%s" (Pipeline_api.Raw.Reader.PackageInfo.name_get pi)

let pp_package_build_status f (ps : Client.Build_status.t) =
  Client.Build_status.pp f ps

let pp_pipeline_health f (h : Pipeline_api.Raw.Reader.PipelineHealth.t) =
  let open Pipeline_api.Raw.Reader.PipelineHealth in
  Fmt.pf f
    "@[<v> Epoch-html: %s \n\
    \ Epoch-linked: %s \n\
    \ Voodoo-go: %s \n\
    \ Voodoo-prep: %s \n\
    \ Voodoo-gen: %s \n\
    \ Failed-packages: %d \n\
    \ Running-packages: %d \n\
    \ Passed-packages: %d@]@." (epoch_html_get h) (epoch_linked_get h)
    (voodoo_do_commit_get h) (voodoo_prep_commit_get h)
    (voodoo_gen_commit_get h)
    (Int64.to_int @@ failing_packages_get h)
    (Int64.to_int @@ running_packages_get h)
    (Int64.to_int @@ passing_packages_get h)

let package_status f ({ version; status } : Client.Package.package_status) =
  Ocolor_format.prettify_formatter f;
  Fmt.pf f "@[%s/%a@] "
    (OpamPackage.Version.to_string version)
    pp_package_build_status status

let list_packages ci =
  Client.Pipeline.packages ci
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "@[<v>No package name given and no suggestions available."
     | packages ->
         Fmt.pr "@[<v>No package name given. Try one of these:@,@,%a@]@."
           Fmt.(list pp_package_info)
           packages

let list_versions_status package_name ?(version = None) package =
  let version = Option.map OpamPackage.Version.of_string version in
  Fmt.pr "@[<v>%s@,@]@." "";
  Fmt.pr "@[<v>package: %s@]@." package_name;

  Fmt.pr "@[<v>Version/Status: @,";
  Client.Package.versions package
  |> Lwt_result.map (fun list' ->
         let list =
           match version with
           | None -> list'
           | Some version' ->
               List.filter
                 (fun ({ version; _ } : Client.Package.package_status) ->
                   version = version')
                 list'
         in

         Fmt.pr "%a@]@." Fmt.(list package_status) list)

let list_versions_status_by_pipeline latest latest_but_one package =
  Client.Package.by_pipeline package latest >>= function
  | Error _ as e -> Lwt.return e
  | Ok latest_packages -> (
      Client.Package.by_pipeline package latest_but_one >>= function
      | Error _ as e -> Lwt.return e
      | Ok latest_but_one_packages ->
          Fmt.pr "%a@]@." Fmt.(list package_status) latest_packages;
          Fmt.pr "%a@]@." Fmt.(list package_status) latest_but_one_packages;
          Lwt.return_ok ())

let list_steps (_package_version : string) package =
  Client.Package.steps package
  |> Lwt_result.map
     @@ fun (package_steps' :
              (string * Client.Build_status.t * Client.Package.step list) list)
       ->
     let package_steps : Client.Package.package_steps_list =
       List.map
         (fun (version, status, steps) : Client.Package.package_steps ->
           { version; status; steps })
         package_steps'
     in
     Fmt.pr "@[<v>%s@]@."
       (package_steps
       |> Client.Package.package_steps_list_to_yojson
       |> Yojson.Safe.to_string)

let main_status ~ci_uri ~package_name ~package_version =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      match package_name with
      | None -> list_packages ci
      | Some package_name ->
          with_ref
            (Client.Pipeline.package ci package_name)
            (list_versions_status package_name ~version:package_version))

let main_list_steps ~ci_uri ~package_name ~package_version =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr ->
      Sturdy_ref.connect_exn sr >>= fun ci ->
      with_ref
        (Client.Pipeline.package ci package_name)
        (list_steps package_version)

let main_health ~ci_uri =
  let pipeline_health f = Fmt.pf f "%a" pp_pipeline_health in
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      Client.Pipeline.pipeline_ids ci >>= function
      | Error _ as e -> Lwt.return e
      | Ok (latest, latest_but_one) -> (
          Client.Pipeline.health ci latest >>= function
          | Error _ as e -> Lwt.return e
          | Ok latest_health -> (
              if latest = latest_but_one then (
                Fmt.pr "@[Only one pipeline has been recorded.@]@.";
                Fmt.pr "%a" pipeline_health latest_health;
                Lwt.return_ok ())
              else
                Client.Pipeline.health ci latest_but_one >>= function
                | Error _ as e -> Lwt.return e
                | Ok latest_but_one_health ->
                    Fmt.pr "Latest: @.%a@]@." pipeline_health latest_health;
                    Fmt.pr "Latest-but-one: @.%a@]@." pipeline_health
                      latest_but_one_health;
                    Lwt.return_ok ())))

let main_diff_pipelines ~ci_uri =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      Client.Pipeline.pipeline_ids ci >>= function
      | Error _ as e -> Lwt.return e
      | Ok (latest, latest_but_one) -> (
          if latest = latest_but_one then (
            Fmt.pr
              "@[Only one pipeline has been recorded. Please try again when a \
               new pipeline has run.@]@.";
            Lwt.return_ok ())
          else
            Client.Pipeline.diff ci latest latest_but_one >>= function
            | Error _ as e -> Lwt.return e
            | Ok failing_packages ->
                if List.length failing_packages = 0 then
                  Fmt.pr
                    "@[<v>Packages that fail in the latest pipeline, that did not fail \
                     in the latest-but-one pipeline:@,\
                     @,\
                     None."
                else
                  Fmt.pr
                    "@[<v>Packages that fail in the latest pipeline, that did not fail \
                     in the latest-but-one pipeline:@,\
                     @,\
                     %a@]@."
                    Fmt.(list pp_package_info)
                    failing_packages;
                Lwt.return_ok ()))

let main_status_by_pipelines ~ci_uri ~package_name =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      Client.Pipeline.pipeline_ids ci >>= function
      | Error _ as e -> Lwt.return e
      | Ok (latest, latest_but_one) -> (
          if latest = latest_but_one then (
            Fmt.pr
              "@[Only one pipeline has been recorded. Please try again when a \
               new pipeline has run.@]@.";
            Lwt.return_ok ())
          else
            match package_name with
            | None -> Lwt.return_error (`Msg "Missing package name")
            | Some package_name ->
                with_ref
                  (Client.Pipeline.package ci package_name)
                  (list_versions_status_by_pipeline latest latest_but_one)))

(* Command-line parsing *)

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const Logging.init
    $ Fmt_cli.style_renderer ~docs ()
    $ Logs_cli.level ~docs ())

let cap =
  Arg.value
  @@ Arg.opt Arg.(some Capnp_rpc_unix.sturdy_uri) None
  @@ Arg.info ~doc:"The ocaml-docs-ci.cap file." ~docv:"CAP" [ "ci-cap" ]

let package =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The Opam package." ~docv:"package" [ "package"; "p" ]

let package_version =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The Opam package version." ~docv:"VERSION"
       [ "version"; "n" ]

let dry_run =
  let info = Arg.info [ "dry-run" ] ~doc:"Dry run (without effect)." in
  Arg.value (Arg.flag info)

type statuscmd_conf = {
  cap : Uri.t option;
  package_name : string option;
  package_version : string option;
  dry_run : bool;
}

type stepscmd_conf = {
  cap : Uri.t option;
  package_name : string option;
  package_version : string option;
  dry_run : bool;
}

type healthcheckcmd_conf = { cap : Uri.t option; dry_run : bool }
type diffpipelinescmd_conf = { cap : Uri.t option; dry_run : bool }

type statusbypipelinecmd_conf = {
  cap : Uri.t option;
  package_name : string option;
  dry_run : bool;
}

type cmd_conf =
  | Status of statuscmd_conf
  | ListSteps of stepscmd_conf
  | HealthCheck of healthcheckcmd_conf
  | DiffPipelines of diffpipelinescmd_conf
  | StatusByPipeline of statusbypipelinecmd_conf

let run cmd_conf =
  match cmd_conf with
  | ListSteps stepscmd_conf -> (
      match stepscmd_conf.dry_run with
      | true ->
          Fmt.pr
            "@[<hov>@,\
            \ DRY RUN -- subcommand:list-steps cap_file: %s package: %s @]@."
            (Option.value ~default:"-"
               (Option.map Uri.to_string stepscmd_conf.cap))
            (Option.value ~default:"-" stepscmd_conf.package_name)
      | false ->
          let main () ci_uri package_name package_version =
            match
              Lwt_main.run
                (main_list_steps ~ci_uri ~package_name ~package_version)
            with
            | Ok _ -> ()
            | Error (`Capnp ex) ->
                Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
                exit 1
            | Error (`Msg m) ->
                Fmt.epr "%s@." m;
                exit 1
          in
          let package' = Option.value ~default:"-" stepscmd_conf.package_name in
          let version' =
            Option.value ~default:"-" stepscmd_conf.package_version
          in
          main () stepscmd_conf.cap package' version')
  | Status statuscmd_conf -> (
      match statuscmd_conf.dry_run with
      | true ->
          Fmt.pr
            "@[<hov>@,\
            \ DRY RUN -- subcommand:status cap_file: %s package_name: %s \
             package_version: %s@,\
             @]@."
            (Option.value ~default:"-"
               (Option.map Uri.to_string statuscmd_conf.cap))
            (Option.value ~default:"-" statuscmd_conf.package_name)
            (Option.value ~default:"-" statuscmd_conf.package_version)
      | false ->
          let main () ci_uri package_name package_version =
            match
              Lwt_main.run (main_status ~ci_uri ~package_name ~package_version)
            with
            | Ok () -> ()
            | Error (`Capnp ex) ->
                Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
                exit 1
            | Error (`Msg m) ->
                Fmt.epr "%s@." m;
                exit 1
          in
          main () statuscmd_conf.cap statuscmd_conf.package_name
            statuscmd_conf.package_version)
  | HealthCheck healthcheckcmd_conf -> (
      match healthcheckcmd_conf.dry_run with
      | true ->
          Fmt.pr
            "@[<hov>@, DRY RUN -- subcommand:health-check cap_file: %s @]@."
            (Option.value ~default:"-"
               (Option.map Uri.to_string healthcheckcmd_conf.cap))
      | false ->
          let main () ci_uri =
            match Lwt_main.run (main_health ~ci_uri) with
            | Ok () -> ()
            | Error (`Capnp ex) ->
                Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
                exit 1
            | Error (`Msg m) ->
                Fmt.epr "%s@." m;
                exit 1
          in
          main () healthcheckcmd_conf.cap)
  | DiffPipelines diffpipelinescmd_conf -> (
      match diffpipelinescmd_conf.dry_run with
      | true ->
          Fmt.pr
            "@[<hov>@, DRY RUN -- subcommand:diff-pipelines cap_file: %s @]@."
            (Option.value ~default:"-"
               (Option.map Uri.to_string diffpipelinescmd_conf.cap))
      | false ->
          let main () ci_uri =
            match Lwt_main.run (main_diff_pipelines ~ci_uri) with
            | Ok () -> ()
            | Error (`Capnp ex) ->
                Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
                exit 1
            | Error (`Msg m) ->
                Fmt.epr "%s@." m;
                exit 1
          in
          main () diffpipelinescmd_conf.cap)
  | StatusByPipeline statusbypipelinecmd_conf -> (
      match statusbypipelinecmd_conf.dry_run with
      | true ->
          Fmt.pr
            "@[<hov>@,\
            \ DRY RUN -- subcommand:status-by-pipeline cap_file: %s \
             package_name: %s @]@."
            (Option.value ~default:"-"
               (Option.map Uri.to_string statusbypipelinecmd_conf.cap))
            (Option.value ~default:"-" statusbypipelinecmd_conf.package_name)
      | false ->
          let main () ci_uri package_name =
            match
              Lwt_main.run (main_status_by_pipelines ~ci_uri ~package_name)
            with
            | Ok () -> ()
            | Error (`Capnp ex) ->
                Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
                exit 1
            | Error (`Msg m) ->
                Fmt.epr "%s@." m;
                exit 1
          in
          main () statusbypipelinecmd_conf.cap
            statusbypipelinecmd_conf.package_name)

let statuscmd_term run =
  let combine () dry_run cap package_name package_version =
    Status { dry_run; cap; package_name; package_version } |> run
  in
  Term.(const combine $ setup_log $ dry_run $ cap $ package $ package_version)

let statuscmd_doc = "Build status of a package."

let statuscmd_man =
  [
    `S Manpage.s_description;
    `P "Lookup the build status of the versions of a package.";
  ]

let statuscmd run =
  let info = Cmd.info "status" ~doc:statuscmd_doc ~man:statuscmd_man in
  Cmd.v info (statuscmd_term run)

let stepscmd_term run =
  let combine () dry_run cap package_name package_version =
    ListSteps { dry_run; cap; package_name; package_version } |> run
  in
  Term.(const combine $ setup_log $ dry_run $ cap $ package $ package_version)

let stepscmd_doc = "Build steps of a package."

let stepscmd_man =
  [ `S Manpage.s_description; `P "Lookup the build steps of the package." ]

let stepscmd run =
  let info = Cmd.info "steps" ~doc:stepscmd_doc ~man:stepscmd_man in
  Cmd.v info (stepscmd_term run)

let healthcheckcmd_term run =
  let combine () dry_run cap = HealthCheck { dry_run; cap } |> run in
  Term.(const combine $ setup_log $ dry_run $ cap)

let healthcheckcmd_doc = "Information about a pipeline."

let healthcheckcmd_man =
  [ `S Manpage.s_description; `P "Get information about a pipeline run." ]

let healthcmd run =
  let info =
    Cmd.info "health-check" ~doc:healthcheckcmd_doc ~man:healthcheckcmd_man
  in
  Cmd.v info (healthcheckcmd_term run)

let diffcmd_term run =
  let combine () dry_run cap = DiffPipelines { dry_run; cap } |> run in
  Term.(const combine $ setup_log $ dry_run $ cap)

let diffcmd_doc = "Packages that have started failing in the latest pipeline."

let diffcmd_man =
  [
    `S Manpage.s_description;
    `P
      "List packages that have failed in the latest pipeline run that passed \
       in the latest-but-one pipeline run.";
  ]

let diffcmd run =
  let info = Cmd.info "diff-pipelines" ~doc:diffcmd_doc ~man:diffcmd_man in
  Cmd.v info (diffcmd_term run)

let statusbypipelinecmd_term run =
  let combine () dry_run cap package_name =
    StatusByPipeline { dry_run; cap; package_name } |> run
  in
  Term.(const combine $ setup_log $ dry_run $ cap $ package)

let statusbypipelinecmd_doc =
  "Build status of a package in the two most recent pipeline runs."

let statusbypipelinecmd_man =
  [
    `S Manpage.s_description;
    `P "Build status of a package in the two most recent pipeline runs.";
  ]

let statusbypipelinecmd run =
  let info =
    Cmd.info "status-by-pipeline" ~doc:statusbypipelinecmd_doc
      ~man:statusbypipelinecmd_man
  in
  Cmd.v info (statusbypipelinecmd_term run)

(*** Putting together the main command ***)

let root_doc = "Cli client for ocaml-docs-ci."

let root_man =
  [ `S Manpage.s_description; `P "Command line client for ocaml-docs-ci." ]

let root_info = Cmd.info "ocaml-docs-ci-client" ~doc:root_doc ~man:root_man

let subcommands run =
  [
    statuscmd run;
    stepscmd run;
    healthcmd run;
    diffcmd run;
    statusbypipelinecmd run;
  ]

let parse_command_line_and_run (run : cmd_conf -> unit) =
  Cmd.group root_info (subcommands run) |> Cmd.eval |> exit

let main () = parse_command_line_and_run run
let () = main ()
