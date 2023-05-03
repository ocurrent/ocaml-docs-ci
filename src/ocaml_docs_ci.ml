open Lwt.Infix
open Capnp_rpc_lwt
module Git = Current_git

let () = Logging.init ()
let hourly = Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ()
let program_name = "ocaml-docs-ci"

(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ -> (
      match Option.map Current_web.User.id user with
      | Some
          ( "github:talex5" | "github:avsm" | "github:kit-ty-kate" | "github:samoht"
          | "github:tmcgilchrist" | "github:dra27" | "github:jonludlam" | "github:TheLortex"
          | "github:sabine" | "github:novemberkilo" ) ->
          true
      | _ -> false)

let or_die = function Ok x -> x | Error (`Msg m) -> failwith m

let check_dir x =
  Lwt.catch
    (fun () ->
      Lwt_unix.stat x >|= function
      | Unix.{ st_kind = S_DIR; _ } -> `Present
      | _ -> Fmt.failwith "Exists, but is not a directory: %S" x)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return `Missing
      | exn -> Lwt.fail exn)

let ensure_dir path =
  check_dir path >>= function
  | `Present ->
      Logs.info (fun f -> f "Directory %s exists" path);
      Lwt.return_unit
  | `Missing ->
      Logs.info (fun f -> f "Creating %s directory" path);
      Lwt_unix.mkdir path 0o777

let run_capnp capnp_public_address capnp_listen_address =
  match (capnp_public_address, capnp_listen_address) with
  | None, None -> Lwt.return (Capnp_rpc_unix.client_only_vat (), None)
  | Some _, None ->
      Lwt.fail_invalid_arg
        "Public address for Cap'n Proto RPC can't be set without setting a \
         capnp-listen-address to listen on."
  | Some _, Some _ | None, Some _ ->
      let ci_profile =
        match Sys.getenv_opt "CI_PROFILE" with
        | Some "production" | None -> `Production
        | Some "dev" -> `Dev
        | Some x -> Fmt.failwith "Unknown $CI_PROFILE setting %S." x
      in
      let cap_secrets =
        match ci_profile with
        | `Production -> "/capnp-secrets"
        | `Dev -> "./capnp-secrets"
      in
      let secret_key = cap_secrets ^ "/secret-key.pem" in
      let cap_file = cap_secrets ^ "/ocaml-docs-ci.cap" in
      let internal_port = 9000 in

      let listen_address =
        match capnp_listen_address with
        | Some listen_address -> listen_address
        | None ->
            Capnp_rpc_unix.Network.Location.tcp ~host:"0.0.0.0"
              ~port:internal_port
      in
      let public_address =
        match capnp_public_address with
        | None -> listen_address
        | Some public_address -> public_address
      in
      ensure_dir cap_secrets >>= fun () ->
      let config =
        Capnp_rpc_unix.Vat_config.create ~public_address
          ~secret_key:(`File secret_key) listen_address
      in
      let rpc_engine, rpc_engine_resolver = Capability.promise () in
      let service_id = Capnp_rpc_unix.Vat_config.derived_id config "ci" in
      let restore = Capnp_rpc_net.Restorer.single service_id rpc_engine in
      Capnp_rpc_unix.serve config ~restore >>= fun vat ->
      Capnp_rpc_unix.Cap_file.save_service vat service_id cap_file |> or_die;
      Logs.app (fun f -> f "Wrote capability reference to %S" cap_file);
      Lwt.return (vat, Some rpc_engine_resolver)

let main current_config github_auth mode capnp_public_address
    capnp_listen_address config =
  ignore
  @@ Logging.run
       (let () =
          match Docs_ci_lib.Init.setup (Docs_ci_lib.Config.ssh config) with
          | Ok () -> ()
          | Error (`Msg msg) ->
              Docs_ci_lib.Log.err (fun f ->
                  f "Failed to initialize the storage server:\n%s" msg);
              exit 1
        in
        run_capnp capnp_public_address capnp_listen_address
        >>= fun (_vat, rpc_engine_resolver) ->
        let repo_opam =
          Git.clone ~schedule:hourly
            "https://github.com/ocaml/opam-repository.git"
        in
        let monitor = Docs_ci_lib.Monitor.make () in
        let engine =
          Current.Engine.create ~config:current_config (fun () ->
              Docs_ci_pipelines.Docs.v ~config ~opam:repo_opam ~monitor ()
              |> Current.ignore_value)
        in
        rpc_engine_resolver
        |> Option.iter (fun r ->
               Capability.resolve_ok r
                 (Docs_ci_pipelines.Api_impl.make ~monitor));

        let has_role =
          if github_auth = None then Current_web.Site.allow_all else has_role
        in
        let secure_cookies = github_auth <> None in
        let authn = Option.map Current_github.Auth.make_login_uri github_auth in
        let site =
          let routes =
            Routes.(
              (s "login" /? nil) @--> Current_github.Auth.login github_auth)
            :: Current_web.routes engine
            @ Docs_ci_lib.Monitor.routes monitor engine
          in
          Current_web.Site.(v ?authn ~has_role ~secure_cookies)
            ~name:program_name routes
        in
        Lwt.choose
          [
            Current.Engine.thread engine;
            (* The main thread evaluating the pipeline. *)
            Current_web.run ~mode site (* Optional: provides a web UI *);
          ])

(* Command-line parsing *)

open Cmdliner

let capnp_public_address =
  Arg.value
  @@ Arg.opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
  @@ Arg.info
       ~doc:
         "Public address (SCHEME:HOST:PORT) for Cap'n Proto RPC (default: no \
          RPC).\n\
         \          If --capnp-listen-address isn't set it will run no RPC."
       ~docv:"ADDR" [ "capnp-public-address" ]

let capnp_listen_address =
  let i =
    Arg.info ~docv:"ADDR"
      ~doc:
        "Address to listen on, e.g. $(b,unix:/run/my.socket) (default: no RPC)."
      [ "capnp-listen-address" ]
  in
  Arg.(
    value
    @@ opt (Arg.some Capnp_rpc_unix.Network.Location.cmdliner_conv) None
    @@ i)

let cmd =
  let doc = "An OCurrent pipeline" in
  let info = Cmd.info program_name ~doc in
  Cmd.v info
    Term.(
      const main
      $ Current.Config.cmdliner
      $ Current_github.Auth.cmdliner
      $ Current_web.cmdliner
      $ capnp_public_address
      $ capnp_listen_address
      $ Docs_ci_lib.Config.cmdliner)

let () = exit @@ Cmd.eval cmd
