module Git = Current_git

let () = Logging.init ()

let monthly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) ()

let program_name = "docs-ci"

let main config mode =
  let repo_opam = Git.clone ~schedule:monthly "https://github.com/ocaml/opam-repository.git" in
  let api = Docs_ci_lib.Web.make () in
  let engine =
    Current.Engine.create ~config (fun () ->
        Docs_ci_pipelines.Docs.v ~api ~opam:repo_opam () |> Current.ignore_value)
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
         Docs_ci_lib.Web.serve api |> Lwt.map Result.ok;
       ])

(* Command-line parsing *)

open Cmdliner

let cmd =
  let doc = "an OCurrent pipeline" in
  (Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner), Term.info program_name ~doc)

let () = Term.(exit @@ eval cmd)
