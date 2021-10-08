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
          | "github:tmcgilchrist" | "github:dra27" | "github:jonludlam" | "github:TheLortex" ) ->
          true
      | _ -> false )

let main current_config github_auth mode config =
  let () =
    match Docs_ci_lib.Init.setup (Docs_ci_lib.Config.ssh config) with
    | Ok () -> ()
    | Error (`Msg msg) ->
        Docs_ci_lib.Log.err (fun f -> f "Failed to initialize the storage server:\n%s" msg);
        exit 1
  in
  let repo_opam = Git.clone ~schedule:hourly "https://github.com/ocaml/opam-repository.git" in
  let engine =
    Current.Engine.create ~config:current_config (fun () ->
        Docs_ci_pipelines.Docs.v ~config ~opam:repo_opam () |> Current.ignore_value)
  in
  let has_role = if github_auth = None then Current_web.Site.allow_all else has_role in
  let secure_cookies = github_auth <> None in
  let site =
    let routes =
      Routes.((s "login" /? nil) @--> Current_github.Auth.login github_auth)
      :: Current_web.routes engine
    in
    Current_web.Site.(v ~has_role ~secure_cookies) ~name:program_name routes
  in
  Logging.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site;
         (* Optional: provides a web UI *)
       ])

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main $ Current.Config.cmdliner $ Current_github.Auth.cmdliner $ Current_web.cmdliner
      $ Docs_ci_lib.Config.cmdliner),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
