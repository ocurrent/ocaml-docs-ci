open Mirage_ci_lib
module Github = Current_github
module Git = Current_git
open Current.Syntax

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let program_name = "mirage-ci"

let gh_mirage_skeleton = { Github.Repo_id.owner = "mirage"; name = "mirage-skeleton" }

let gh_mirage_dev = { Github.Repo_id.owner = "mirage"; name = "mirage-dev" }

let main config github mode =
  let repo_mirage_skeleton =
    let+ repo_gh = Github.Api.head_of github gh_mirage_skeleton (`Ref "refs/heads/mirage-4") in
    Github.Api.Commit.id repo_gh
  in
  let repo_mirage_skeleton = Git.fetch repo_mirage_skeleton in
  let repo_mirage_dev =
    let+ repo_gh = Github.Api.head_of github gh_mirage_dev (`Ref "refs/heads/mirage-4") in
    Github.Api.Commit.id repo_gh
  in
  let repo_mirage_dev = Git.fetch repo_mirage_dev in
  let repo_opam =
    Current_git.clone ~schedule:daily "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily "https://github.com/dune-universe/opam-overlays.git"
  in
  let repos =
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
      repo_mirage_dev |> Current.map (fun x -> ("mirage-dev", x));
    ]
    |> Current.list_seq
  in
  let repos_unfetched = Repository.current_list_unfetch repos in
  let repos_mirage_main =
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
    ]
    |> Current.list_seq
  in
  let roots = Universe.Project.packages in
  let monorepo = Monorepo.v ~system:Platform.system ~repos in
  let monorepo_lock =
    Mirage_ci_pipelines.Monorepo.lock ~system:Platform.system ~value:"universe" ~monorepo
      ~repos:repos_unfetched roots
  in
  let mirage_4 =
    Current.with_context repos @@ fun () ->
    let mirage_skeleton_arm64 =
      Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_arm64 ~targets:[ "unix"; "hvt" ]
        ~monorepo ~repos repo_mirage_skeleton
    in
    let mirage_skeleton_amd64 =
      Mirage_ci_pipelines.Skeleton.v_4 ~platform:Platform.platform_amd64 ~targets:[ "xen"; "spt" ]
        ~monorepo ~repos repo_mirage_skeleton
    in
    let mirage_released =
      Mirage_ci_pipelines.Monorepo.released ~platform:Platform.platform_arm64 ~roots
        ~repos:repos_unfetched ~lock:monorepo_lock
    in
    let mirage_edge =
      Mirage_ci_pipelines.Monorepo.mirage_edge ~platform:Platform.platform_arm64
        ~remote_pull:Config.v.remote_pull ~remote_push:Config.v.remote_push ~roots
        ~repos:repos_unfetched ~lock:monorepo_lock
    in
    let universe_edge =
      Mirage_ci_pipelines.Monorepo.universe_edge ~platform:Platform.platform_arm64
        ~remote_pull:Config.v.remote_pull ~remote_push:Config.v.remote_push ~roots
        ~repos:repos_unfetched ~lock:monorepo_lock
    in
    Current.all_labelled
      [
        ("mirage-skeleton-arm64", mirage_skeleton_arm64);
        ("mirage-skeleton-amd64", mirage_skeleton_amd64);
        ("mirage-released", mirage_released);
        ("mirage-edge", mirage_edge);
        ("universe-edge", universe_edge);
      ]
  in
  let prs =
    Mirage_ci_pipelines.PR.make github (Repository.current_list_unfetch repos_mirage_main)
  in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled
          [ ("mirage 4", mirage_4); ("mirage-main-ci", Mirage_ci_pipelines.PR.to_current prs) ])
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
