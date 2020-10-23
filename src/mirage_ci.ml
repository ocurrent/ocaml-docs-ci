(* This is the main entry-point for the executable.
   Edit [cmd] to set the text for "--help" and modify the command-line interface. *)

let () = Logging.init ()

let program_name = "mirage-ci"

let main config mode repo_mirage_skeleton repo_mirage_dev =
  let repo_mirage_skeleton = Current_git.Local.v (Fpath.v repo_mirage_skeleton) in
  let repo_mirage_dev = Current_git.Local.v (Fpath.v repo_mirage_dev) in
  let repo_mirage_ci = Current_git.Local.v (Fpath.v ".") in
  let engine = Current.Engine.create ~config (Pipeline.v ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci) in
  let site = Current_web.Site.(v ~has_role:allow_all) ~name:program_name (Current_web.routes engine) in
  Logging.run begin
    Lwt.choose [
      Current.Engine.thread engine;  (* The main thread evaluating the pipeline. *)
      Current_web.run ~mode site;    (* Optional: provides a web UI *)
    ]
  end

(* Command-line parsing *)

open Cmdliner

(* An example command-line argument: the repository to monitor *)


let mirage_skeleton =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"The mirage-skeleton repository."
    ~docv:"MIRAGE_SKELETON"
    []

let mirage_dev =
  Arg.required @@
  Arg.pos 1 Arg.(some dir) None @@
  Arg.info
    ~doc:"The mirage-dev repository."
    ~docv:"MIRAGE_DEV"
    []

let cmd =
  let doc = "an OCurrent pipeline" in
  Term.(const main $ Current.Config.cmdliner $ Current_web.cmdliner $ mirage_skeleton $ mirage_dev),
  Term.info program_name ~doc

let () = Term.(exit @@ eval cmd)
