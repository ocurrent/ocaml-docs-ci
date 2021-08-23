module Git = Current_git

let () = Logging.init ()

let hourly = Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) ()

let program_name = "ocaml-docs-ci"

let main current_config mode config =
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
  let site =
    let routes = Current_web.routes engine in
    Current_web.Site.(v ~has_role:allow_all) ~name:program_name routes
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
      const main $ Current.Config.cmdliner $ Current_web.cmdliner
      $ Docs_ci_lib.Config.cmdliner),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
