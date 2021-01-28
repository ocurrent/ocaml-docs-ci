let pool = Current.Pool.create ~label:"git clone" 8

open Lwt.Infix
open Current.Syntax

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

module Assembler = struct
  type t = (string * string) list

  let id = "mirage-ci-monorepo-assembler"

  module Key = struct
    type t = {
      monorepo : OpamParserTypes.opamfile;
      projects : (string * Current_git.Commit.t) list;
    }

    let digest { projects; _ } =
      List.map (fun (_, x) -> Current_git.Commit.hash x) projects |> String.concat ";"
  end

  module Value = Current_docker.Default.Image

  let opam_monorepo_spec ~repos =
    let open Obuilder_spec in
    stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
    @@ Setup.install_tools ~repos ~tools:[ "dune" ]
    @ [
        workdir "/src/";
        run "sudo chown opam /src/";
        user ~uid:1000 ~gid:1000;
        copy [ "." ] ~dst:"/src/";
        run "opam pin -n add monorepo . --locked --ignore-pin-depends";
        run "opam depext --update -y monorepo";
        run "opam pin -n remove monorepo";
      ]

  let build repos job { Key.monorepo; projects } =
    Current.Job.start ~level:Harmless job >>= fun () ->
    Current.Process.with_tmpdir (fun monorepo_path ->
        List.map
          (fun (name, commit) ->
            (* setup monorepo *)
            Current_git.with_checkout ~pool ~job commit (fun repo_path ->
                let cmd =
                  Bos.Cmd.(
                    v "cp" % "-r" % Fpath.to_string repo_path
                    % Fpath.(to_string (monorepo_path / name)))
                in
                Bos.OS.Cmd.run cmd |> or_raise;
                Lwt.return_ok ()))
          projects
        |> Lwt.all
        >>= fun _ ->
        Bos.OS.File.write Fpath.(monorepo_path / "monorepo.opam") (OpamPrinter.opamfile monorepo)
        |> or_raise;
        Current.Job.log job "Cloned every git repository in %a" Fpath.pp monorepo_path;
        let dockerfile =
          Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false (opam_monorepo_spec ~repos)
        in
        Bos.OS.File.write Fpath.(monorepo_path / "Dockerfile") dockerfile |> or_raise;
        let iidfile = Fpath.(monorepo_path / "image.id") in
        let cmd =
          Current_docker.Raw.Cmd.docker ~docker_context:None
            [ "build"; "--iidfile"; Fpath.to_string iidfile; "--"; Fpath.to_string monorepo_path ]
        in
        Current.Process.exec ~cancellable:true ~job cmd >|= fun res ->
        Result.bind res (fun () -> Bos.OS.File.read iidfile)
        |> Result.map (fun id -> Current_docker.Default.Image.of_hash id))

  let pp f _ = Fmt.string f "Monorepo assembler"

  let auto_cancel = true
end

module Cache = Current_cache.Make (Assembler)

let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

module String = Astring.String

let parse_opam_dev_repo dev_repo =
  let repo, branch =
    match String.cuts ~sep:"#" dev_repo with
    | [ repo ] -> (repo, None)
    | [ repo; branch ] -> (repo, Some branch)
    | _ -> failwith "String.cuts dev_repo"
  in
  let repo = if String.is_prefix ~affix:"git+" repo then String.drop ~max:4 repo else repo in
  Printf.printf "repo: %s\n" repo;
  (repo, branch)

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let monorepo_main ~analysis ~repos () =
  let projects =
    let* analysis = analysis in
    (* Bind: the list of tracked projects is dynamic *)
    let projects = Analyse.Analysis.projects analysis in
    Printf.printf "got %d projects to track.\n" (List.length projects);
    List.map
      (fun (x : Analyse.Analysis.project) ->
        let repo_url, repo_branch = parse_opam_dev_repo x.dev_repo in
        let+ commit = Current_git.clone ~schedule:daily ?gref:repo_branch repo_url in
        (x.name, commit))
      projects
    |> Current.list_seq
  in
  Current.component "Monorepo assembler"
  |> let> projects = projects and> analysis = analysis and> repos = Current.list_seq repos in
     let repos = List.map (fun (a, b) -> (a, remote_uri b)) repos in
     Cache.get repos { projects; monorepo = Analyse.Analysis.lockfile analysis }
