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

(* let pp_project_build_status f
    (ps : Pipeline_api.Raw.Reader.ProjectBuildStatus.t) =
  Client.Build_status.pp f
  @@ Pipeline_api.Raw.Reader.ProjectBuildStatus.status_get ps *)

let list_projects ci =
  Client.Pipeline.projects ci
  |> Lwt_result.map @@ function
     | [] -> Fmt.pr "@[<v>No project name given and no suggestions available."
     | orgs ->
         Fmt.pr "@[<v>No project name given. Try one of these:@,@,%a@]@."
           Fmt.(list pp_project_info)
           orgs

(* let show_status ci project_name project_version =
   Client.Pipeline.status ci project_name project_version
   |> Lwt_result.map @@ fun status ->
      Fmt.pr "@[<v> @,@,%a@]@." pp_project_build_status status *)

let list_versions project_name project =
  Printf.printf "\n";
  Client.Project.versions project ()
  |> Lwt_result.map (fun list ->
         let project_version f ({ version } : Client.Project.project_version) =
           Fmt.pf f "%s/%s" project_name version
         in
         Fmt.pr "%a." Fmt.(list project_version) list)

let main ~ci_uri ~project_name ~project_version =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      match project_name with
      | None -> list_projects ci
      | Some repo -> (
          match project_version with
          | None ->
              with_ref (Client.Pipeline.project ci repo) (list_versions repo)
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

let cmd =
  let doc = "Client for ocaml-docs-ci" in
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
  let info = Cmd.info "ocaml-docs-ci" ~doc in
  Cmd.v info
    Term.(const main $ setup_log $ cap $ project_name $ project_version)

let () = exit @@ Cmd.eval cmd
