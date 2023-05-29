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

let pp_project_info f (pi : Pipeline_api.Raw.Reader.ProjectInfo.t) =
  Fmt.pf f "%s" (Pipeline_api.Raw.Reader.ProjectInfo.name_get pi)

let pp_project_build_status f (ps : Client.Build_status.t) =
  Client.Build_status.pp f ps

let list_projects ci =
  Client.Pipeline.projects ci
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "@[<v>No project name given and no suggestions available."
     | orgs ->
         Fmt.pr "@[<v>No project name given. Try one of these:@,@,%a@]@."
           Fmt.(list pp_project_info)
           orgs

let list_versions_status project_name project =
  Fmt.pr "@[<v>%s@,@]@." "";
  Fmt.pr "@[<v>Project: %s@]@." project_name;

  Fmt.pr "@[<hov>Version/Status: @,";
  Client.Project.status project
  |> Lwt_result.map (fun list ->
         let project_status f
             ({ version; status } : Client.Project.project_status) =
           Ocolor_format.prettify_formatter f;
           Fmt.pf f "@[%s/%a@] "
             (OpamPackage.Version.to_string version)
             pp_project_build_status status
         in
         Fmt.pr "%a@]@." Fmt.(list project_status) list)

let main ~ci_uri ~project_name ~project_version =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      match project_name with
      | None -> list_projects ci
      | Some project_name -> (
          match project_version with
          | None ->
              with_ref
                (Client.Pipeline.project ci project_name)
                (list_versions_status project_name)
          | Some _version -> Lwt_result.fail (`Msg "unimplemented")))

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

let project_name =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The Opam Project name." ~docv:"PROJECT" [ "project"; "p" ]

let project_version =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The Opam Project version." ~docv:"VERSION"
       [ "version"; "n" ]

type statuscmd_conf = {
  cap : Uri.t option;
  project_name : string option;
  project_version : string option;
}

type cmd_conf = Status of statuscmd_conf

let run cmd_conf =
  match cmd_conf with
  | Status statuscmd_conf ->
      let main () ci_uri project_name project_version =
        match Lwt_main.run (main ~ci_uri ~project_name ~project_version) with
        | Ok () -> ()
        | Error (`Capnp ex) ->
            Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
            exit 1
        | Error (`Msg m) ->
            Fmt.epr "%s@." m;
            exit 1
      in
      main () statuscmd_conf.cap statuscmd_conf.project_name
        statuscmd_conf.project_version

let statuscmd_term run =
  let combine () cap project_name project_version =
    Status { cap; project_name; project_version } |> run
  in
  Term.(const combine $ setup_log $ cap $ project_name $ project_version)

let statuscmd_doc = "[Some headline for status]"

let statuscmd_man =
  [ `S Manpage.s_description; `P "[multiline overview of statuscmd]" ]

let statuscmd run =
  let info = Cmd.info "status" ~doc:statuscmd_doc ~man:statuscmd_man in
  Cmd.v info (statuscmd_term run)
(*** Putting together the main command ***)

let root_doc = "[some headline for the main command]"

let root_man =
  [ `S Manpage.s_description; `P "[multiline overview of the main command]" ]

let root_info = Cmd.info "ocaml-docs-ci-client" ~doc:root_doc ~man:root_man
let subcommands run = [ statuscmd run ]

let parse_command_line_and_run (run : cmd_conf -> unit) =
  Cmd.group root_info (subcommands run) |> Cmd.eval |> exit

let main () = parse_command_line_and_run run
let () = main ()
