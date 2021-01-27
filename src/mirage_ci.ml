(* This is the main entry-point for the executable.
   Edit [cmd] to set the text for "--help" and modify the command-line interface. *)

let () = Logging.init ()

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let program_name = "mirage-ci"

open Current.Syntax

module String = Astring.String

let parse_opam_dev_repo dev_repo =
  let repo, branch = match String.cuts ~sep:"#" dev_repo with 
    | [repo] -> repo, None
    | [repo; branch] -> repo, Some branch
    | _ -> failwith "String.cuts dev_repo"
  in
  let repo = if String.is_prefix ~affix:"git+" repo then String.drop ~max:4 repo else repo in 
  Printf.printf "repo: %s\n" repo;
  repo, branch

let main config mode repo_mirage_skeleton repo_mirage_dev =
  let repo_mirage_skeleton =
    Current_git.Local.v (Fpath.v repo_mirage_skeleton)
  in
  let repo_mirage_dev = Current_git.Local.v (Fpath.v repo_mirage_dev) in
  let repo_mirage_ci = Current_git.Local.v (Fpath.v ".") in
  let repo_opam =
    Current_git.clone ~schedule:daily
      "https://github.com/ocaml/opam-repository.git"
  in
  let repo_overlays =
    Current_git.clone ~schedule:daily ~gref:"add-hmap-0.8.0"
      "https://github.com/TheLortex/opam-overlays.git"
  in
  let repos =  
    [
      repo_opam |> Current.map (fun x -> ("opam", x));
      repo_overlays |> Current.map (fun x -> ("overlays", x));
    ]
  in
  let _mirage_skeleton =
    Pipeline.v ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci
  in
  let _mirage_universe =
    Pipeline.v2 ~repo_mirage_dev ~repo_opam ~repo_overlays
      Universe.Project.packages
  in
  let mirage_analyzer =
    let analyzer =
      Analyse.v
        ~repos
        ~packages:Universe.Project.packages ()
    in
    Current.component "the Universe" |>
    let** packages = analyzer in 
      Printf.printf "got %d projects to track.\n" (List.length packages);
      let projects_master = List.map (fun (x: Analyse.project) -> 
        let repo_url, repo_branch = parse_opam_dev_repo x.dev_repo in
        let+ commit = Current_git.clone ~schedule:daily ?gref:repo_branch repo_url in 
        (x.name, commit)
      ) packages
      in 
      let image = Monorepo.monorepo_master ~projects:projects_master ~repos () in 
      [
        Current_docker.Default.run ~label:"dune build" ~args:["opam"; "exec"; "--"; "dune"; "build"; "@install"] image;
      ]
      |> Current.all
  in
  let engine =
    Current.Engine.create ~config (fun () ->
        Current.all_labelled
          [
            (*("mirage-skeleton", mirage_skeleton ());
              ("mirage-universe", mirage_universe);*)
            ("mirage-universe", mirage_analyzer);
          ])
  in
  let site =
    Current_web.Site.(v ~has_role:allow_all)
      ~name:program_name
      (Current_web.routes engine)
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

let mirage_skeleton =
  Arg.required
  @@ Arg.pos 0 Arg.(some dir) None
  @@ Arg.info ~doc:"The mirage-skeleton repository." ~docv:"MIRAGE_SKELETON" []

let mirage_dev =
  Arg.required
  @@ Arg.pos 1 Arg.(some dir) None
  @@ Arg.info ~doc:"The mirage-dev repository." ~docv:"MIRAGE_DEV" []

let cmd =
  let doc = "an OCurrent pipeline" in
  ( Term.(
      const main $ Current.Config.cmdliner $ Current_web.cmdliner
      $ mirage_skeleton $ mirage_dev),
    Term.info program_name ~doc )

let () = Term.(exit @@ eval cmd)
