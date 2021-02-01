open Mirage_ci_lib
open Mirage_ci_pipelines

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let program_name = "mirage-ci"

let main config mode =
  let repo_mirage_skeleton =
    Current_git.clone ~schedule:daily "https://github.com/TheLortex/mirage-skeleton.git"
  in
  let repo_opam =
    Current_git.clone ~schedule:daily "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily "https://github.com/dune-universe/opam-overlays.git"
  in
  let repo_mirage_dev =
    Current_git.clone ~schedule:daily ~gref:"mirage-4" "https://github.com/TheLortex/mirage-dev.git"
  in
  let repos =
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
      repo_mirage_dev |> Current.map (fun x -> ("mirage-dev", x));
    ]
  in
  let mirage_skeleton = Pipelines.skeleton ~repos repo_mirage_skeleton in
  let mirage_released = Pipelines.monorepo_released ~repos Universe.Project.packages in
  let mirage_edge = Pipelines.monorepo_edge ~repos Universe.Project.packages in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled
          [
            ("mirage-skeleton", mirage_skeleton);
            ("mirage-released", mirage_released);
            ("mirage-edge", mirage_edge);
          ])
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine)
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

(* An example command-line argument: the repository to monitor *)

let cmd =
  let doc = "an OCurrent pipeline" in
  (Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner), Term.info program_name ~doc)

let () = Term.(exit @@ eval cmd)
