open Mirage_ci_lib
module Github = Current_github
module Git = Current_git

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let program_name = "mirage-ci"

let main config github mode =
  let repo_mirage_skeleton =
    Current_git.clone ~schedule:daily "https://github.com/mirage/mirage-skeleton.git"
  in
  let repo_opam =
    Current_git.clone ~schedule:daily "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily "https://github.com/dune-universe/opam-overlays.git"
  in
  let repo_mirage_dev =
    Current_git.clone ~schedule:daily ~gref:"mirage-4" "https://github.com/mirage/mirage-dev.git"
  in
  let repos =
    [
      repo_opam |> Current.map (fun x -> ("opam", Current_git.Commit.id x));
      repo_overlays |> Current.map (fun x -> ("overlays", Current_git.Commit.id x));
      repo_mirage_dev |> Current.map (fun x -> ("mirage-dev", Current_git.Commit.id x));
    ]
    |> Current.list_seq
  in
  let repos_mirage_main =
    [
      repo_opam |> Current.map (fun x -> ("opam", Current_git.Commit.id x));
      repo_overlays |> Current.map (fun x -> ("overlays", Current_git.Commit.id x));
    ]
    |> Current.list_seq
  in
  let roots = Universe.Project.packages in
  let monorepo = Monorepo.v ~system:Platform.system ~repos in
  let monorepo_lock =
    Mirage_ci_pipelines.Monorepo.lock ~system:Platform.system ~value:"universe" ~monorepo ~repos
      roots
  in
  let mirage_skeleton_arm64 =
    Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_arm64 ~targets:[ "unix"; "hvt" ]
      ~monorepo ~repos repo_mirage_skeleton
  in
  let mirage_skeleton_amd64 =
    Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_amd64 ~targets:[ "xen"; "spt" ]
      ~monorepo ~repos repo_mirage_skeleton
  in
  let mirage_released =
    Mirage_ci_pipelines.Monorepo.released ~platform:Platform.platform_arm64 ~roots ~repos
      ~lock:monorepo_lock
  in
  let mirage_edge =
    Mirage_ci_pipelines.Monorepo.mirage_edge ~platform:Platform.platform_arm64
      ~remote_pull:Config.v.remote_pull ~remote_push:Config.v.remote_push ~roots ~repos
      ~lock:monorepo_lock
  in
  let universe_edge =
    Mirage_ci_pipelines.Monorepo.universe_edge ~platform:Platform.platform_arm64
      ~remote_pull:Config.v.remote_pull ~remote_push:Config.v.remote_push ~roots ~repos
      ~lock:monorepo_lock
  in
  let prs = Mirage_ci_pipelines.PR.make github repos_mirage_main in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled
          [
            ("mirage-skeleton-arm64", mirage_skeleton_arm64);
            ("mirage-skeleton-amd64", mirage_skeleton_amd64);
            ("mirage-released", mirage_released);
            ("mirage-edge", mirage_edge);
            ("universe-edge", universe_edge);
            ("mirage-main-ci", Mirage_ci_pipelines.PR.to_current prs);
          ])
  in
  let site =
    let routes =
      Routes.((s "webhooks" / s "github" /? nil) @--> Github.webhook)
      :: Mirage_ci_pipelines.PR.routes prs
      @ Current_web.routes engine
    in
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
  ( Term.(const main $ Current.Config.cmdliner $ Current_github.Api.cmdliner $ Current_web.cmdliner),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
