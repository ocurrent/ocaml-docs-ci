open Mirage_ci_lib

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
    |> Current.list_seq
  in
  let roots = Universe.Project.packages in
  let monorepo = Monorepo.v ~repos in
  let monorepo_lock = Mirage_ci_pipelines.Monorepo.lock ~value:"universe" ~monorepo ~repos roots in
  let mirage_skeleton = Mirage_ci_pipelines.Skeleton.v ~monorepo ~repos repo_mirage_skeleton in
  let mirage_released = Mirage_ci_pipelines.Monorepo.released ~roots ~repos ~lock:monorepo_lock in
  let mirage_edge =
    Mirage_ci_pipelines.Monorepo.edge ~remote_pull:Config.v.remote_pull
      ~remote_push:Config.v.remote_push ~roots ~repos ~lock:monorepo_lock
  in
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

let cmd =
  let doc = "an OCurrent pipeline" in
  (Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner), Term.info program_name ~doc)

let () = Term.(exit @@ eval cmd)
