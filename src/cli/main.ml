open Astring
open Lwt.Infix
open Capnp_rpc_lwt
module Client = Pipeline_api.Client

let errorf msg = msg |> Fmt.kstr @@ fun msg -> Error (`Msg msg)

let import_ci_ref ~vat = function
  | Some url -> Capnp_rpc_unix.Vat.import vat url
  | None -> (
      match Sys.getenv_opt "HOME" with
      | None -> errorf "$HOME not set! Can't get default cap file location.@."
      | Some home ->
          let path = Filename.concat home ".ocaml-ci.cap" in
          if Sys.file_exists path then Capnp_rpc_unix.Cap_file.load vat path
          else errorf "Default cap file %S not found!" path)

let list_projects ci =
  Client.Pipeline.projects ci
  |> Lwt_result.map @@ function
     | [] ->
         Fmt.pr
           "@[<v>No project name given and no suggestions available."
     | orgs ->
         Fmt.pr
           "@[<v>No project name given. Try one of these:@,@,%a@]@."
           Fmt.(list string)
           orgs

let list_versions ci project =
  Client.

let main ~ci_uri ~repo =
  let vat = Capnp_rpc_unix.client_only_vat () in
  match import_ci_ref ~vat ci_uri with
  | Error _ as e -> Lwt.return e
  | Ok sr -> (
      Sturdy_ref.connect_exn sr >>= fun ci ->
      match repo with
      | None -> list_projects ci
      | Some repo -> (
        with_ref (Client.Pipeline.project ci repo) (list_versions repo))

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
  @@ Arg.info ~doc:"The ocaml-ci.cap file." ~docv:"CAP" [ "ci-cap" ]

let repo =
  Arg.value
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The GitHub repository to use (org/name)." ~docv:"REPO" []

let gref =
  let make_ref s =
    if String.is_prefix ~affix:"refs/pull/" s then
      match String.cuts ~sep:"/" s with
      | [ "refs"; "pull"; pr ] -> Ok (`Ref (Fmt.str "refs/pull/%s/head" pr))
      | _ -> Ok (`Ref s)
    else Ok (`Ref s)
  in
  let parse s =
    if not (Stdlib.String.contains s '/') then
      if String.length s < 6 then
        Error
          (`Msg
            "Git reference should start 'refs/' or be a hash at least 6 \
             characters long")
      else Ok (`Commit s)
    else if String.is_prefix ~affix:"refs/" s then make_ref s
    else make_ref ("refs/" ^ s)
  in
  let pp f = function
    | `Commit s -> Fmt.string f s
    | `Ref r -> Fmt.string f r
  in
  Arg.conv (parse, pp)

let target =
  Arg.value
  @@ Arg.pos 1 Arg.(some gref) None
  @@ Arg.info
       ~doc:
         "The branch, commit or pull request to use. e.g. heads/master or \
          pull/3"
       ~docv:"TARGET" []

let variant =
  Arg.value
  @@ Arg.pos 2 Arg.(some string) None
  @@ Arg.info ~doc:"The build matrix variant" ~docv:"VARIANT" []

let job_op =
  let ops =
    [
      ("log", `Show_log);
      ("status", `Show_status);
      ("cancel", `Cancel);
      ("rebuild", `Rebuild);
    ]
  in
  Arg.value
  @@ Arg.pos 3 Arg.(enum ops) `Show_status
  @@ Arg.info ~doc:"The operation to perform (log, status, cancel or rebuild)."
       ~docv:"METHOD" []

(* (cmdliner's [enum] can't cope with functions) *)
let to_fn = function
  | `Cancel -> cancel
  | `Rebuild -> rebuild
  | `Show_log -> show_log
  | `Show_status -> show_status

let cmd =
  let doc = "Client for ocaml-ci" in
  let main () ci_uri repo target variant job_op =
    let job_op = to_fn job_op in
    match Lwt_main.run (main ~ci_uri ~repo ~target ~variant ~job_op) with
    | Ok () -> ()
    | Error (`Capnp ex) ->
        Fmt.epr "%a@." Capnp_rpc.Error.pp ex;
        exit 1
    | Error (`Msg m) ->
        Fmt.epr "%s@." m;
        exit 1
  in
  let info = Cmd.info "ocaml-ci" ~doc in
  Cmd.v info
    Term.(const main $ setup_log $ cap $ repo $ target $ variant $ job_op)

let () = exit @@ Cmd.eval cmd
