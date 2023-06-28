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

let list_packages ci =
  Client.Pipeline.packages ci
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "@[<v>No package name given and no suggestions available."
     | orgs ->
         Fmt.pr "@[<v>No package name given. Try one of these:@,@,%a@]@."
           Fmt.(list pp_package_info)
           orgs

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
         let package_status f
             ({ version; status } : Client.Package.package_status) =
           Ocolor_format.prettify_formatter f;
           Fmt.pf f "@[%s/%a@] "
             (OpamPackage.Version.to_string version)
             pp_package_build_status status
         in
         Fmt.pr "%a@]@." Fmt.(list package_status) list)

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

type cmd_conf = Status of statuscmd_conf | ListSteps of stepscmd_conf

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

(*** Putting together the main command ***)

let root_doc = "Cli client for ocaml-docs-ci."

let root_man =
  [ `S Manpage.s_description; `P "Command line client for ocaml-docs-ci." ]

let root_info = Cmd.info "ocaml-docs-ci-client" ~doc:root_doc ~man:root_man
let subcommands run = [ statuscmd run; stepscmd run ]

let parse_command_line_and_run (run : cmd_conf -> unit) =
  Cmd.group root_info (subcommands run) |> Cmd.eval |> exit

let main () = parse_command_line_and_run run
let () = main ()
