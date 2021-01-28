open Lwt.Infix
open Current.Syntax

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

let ( let>> ) = Lwt_result.bind

module Analysis = struct
  module OpamParserType = struct
    type opamfile = OpamParserTypes.opamfile

    let opamfile_to_yojson f = `String (OpamPrinter.opamfile f)

    let opamfile_of_yojson = function
      | `String s -> Ok (OpamParser.string s "")
      | _ -> Error "failed to parse opamfile"
  end

  type t = { lockfile : OpamParserType.opamfile; dev_repos_output : string list }
  [@@deriving yojson]

  let marshal t = to_yojson t |> Yojson.Safe.to_string

  let unmarshal s =
    match Yojson.Safe.from_string s |> of_yojson with Ok x -> x | Error e -> failwith e

  type project = { name : string; dev_repo : string; repo : string; packages : string list }

  let lockfile t = t.lockfile

  let clean = Astring.String.trim ~drop:(function ' ' | '\t' | '"' -> true | _ -> false)

  let build_project_list (packages : Opamfile.pkg list) dev_repos_output =
    let module StringMap = Map.Make (String) in
    let repo_map = ref StringMap.empty in
    List.iter
      (fun (pkg : Opamfile.pkg) ->
        match StringMap.find_opt pkg.repo !repo_map with
        | Some pkgs -> repo_map := StringMap.add pkg.repo (pkg :: pkgs) !repo_map
        | None -> repo_map := StringMap.add pkg.repo [ pkg ] !repo_map)
      packages;
    let dev_repo_map = ref StringMap.empty in
    let _ =
      List.fold_left
        (fun name (line : string) ->
          match String.split_on_char ':' line with
          | "name" :: rest -> String.concat ":" rest
          | "dev-repo" :: rest ->
              let dev_repo = String.concat ":" rest in
              dev_repo_map := StringMap.add (clean name) (clean dev_repo) !dev_repo_map;
              ""
          | _ -> "")
        "" dev_repos_output
    in
    StringMap.fold
      (fun repo (pkgs : Opamfile.pkg list) aux ->
        let packages = List.map (fun (pkg : Opamfile.pkg) -> clean pkg.name) pkgs in
        let name =
          List.fold_left
            (fun cur_name name ->
              match cur_name with
              | Some cur_name
                when String.(length cur_name < length name) || StringMap.mem name !dev_repo_map ->
                  Some cur_name
              | _ -> Some name)
            None packages
          |> Option.get
        in
        Printf.printf "%s -> %s\n" name repo;
        { name; dev_repo = StringMap.find name !dev_repo_map; repo = clean repo; packages } :: aux)
      !repo_map []

  let projects t =
    let packages = Opamfile.get_packages t.lockfile in
    build_project_list packages t.dev_repos_output
end

let monorepo_opam_file ~ocaml_version (projects : Universe.Project.t list) =
  let pp_project f (proj : Universe.Project.t) =
    List.iter (fun opam -> Fmt.pf f "\"%s\"\n" opam) proj.opam
  in
  Fmt.str {|
opam-version: "2.0"
depends: [
  "ocaml" { = "%s"}
  %a
]|} ocaml_version
    (Fmt.list pp_project) projects

let generate_monorepo =
  let open Obuilder_spec in
  [
    workdir "/src/";
    run "sudo chown opam /src";
    copy [ "monorepo.opam" ] ~dst:"/src/";
    run "opam monorepo lock";
  ]

module Analyse = struct
  type t = unit

  module Key = struct
    type t = { repos : (string * Current_git.Commit.t) list; packages : Universe.Project.t list }

    let digest t =
      let json =
        `Assoc
          (List.map
             (fun (name, repo) -> ("repo-" ^ name, `String (Current_git.Commit.hash repo)))
             t.repos)
      in
      Yojson.Safe.to_string json
  end

  module Value = Analysis

  let id = "mirage-ci-analysis"

  let remote_uri commit =
    let commit_id = Current_git.Commit.id commit in
    let repo = Current_git.Commit_id.repo commit_id in
    let commit = Current_git.Commit.hash commit in
    repo ^ "#" ^ commit

  let build () job { Key.repos; packages } =
    let repos = List.map (fun (name, commit) -> (name, remote_uri commit)) repos in
    Current.Job.start ~level:Harmless job >>= fun () ->
    let spec =
      Obuilder_spec.stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
      @@ Setup.install_tools ~repos ~tools:[ "opam-monorepo" ]
      @ generate_monorepo
    in
    let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true spec in
    Current.Job.log job "Starting docker build for analysis.";
    let>> id =
      Current.Process.with_tmpdir (fun tmpdir ->
          Bos.OS.File.write Fpath.(tmpdir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
          Bos.OS.File.write
            Fpath.(tmpdir / "monorepo.opam")
            (monorepo_opam_file ~ocaml_version:"4.11.1" packages)
          |> or_raise;
          let iidfile = Fpath.(tmpdir / "image.id") in
          let cmd =
            Current_docker.Raw.Cmd.docker ~docker_context:None
              [ "build"; "--iidfile"; Fpath.to_string iidfile; "--"; Fpath.to_string tmpdir ]
          in
          Current.Process.exec ~cancellable:true ~job cmd >|= fun res ->
          Result.bind res (fun () -> Bos.OS.File.read iidfile))
    in
    let>> lockfile_str =
      let cmd =
        Current_docker.Raw.Cmd.docker ~docker_context:None
          [ "run"; "-i"; id; "cat"; "/src/monorepo.opam.locked" ]
      in
      Current.Process.check_output ~cancellable:true ~job cmd
    in
    let lockfile = OpamParser.string lockfile_str "monorepo.opam.locked" in
    let packages = Opamfile.get_packages lockfile in
    let>> dev_repos_str =
      let cmd =
        Current_docker.Raw.Cmd.docker ~docker_context:None
          ( [ "run"; "-i"; id; "opam"; "show"; "--field"; "name:,dev-repo:" ]
          @ List.map (fun (pkg : Opamfile.pkg) -> pkg.name) packages )
      in
      Current.Process.check_output ~cancellable:true ~job cmd
    in
    Lwt.return_ok { Analysis.lockfile; dev_repos_output = String.split_on_char '\n' dev_repos_str }

  let pp f _ = Fmt.string f "Analyse"

  let auto_cancel = true
end

module Analyse_cache = Current_cache.Make (Analyse)

let v ~repos ~packages ?(with_test = false) () =
  if with_test then failwith "not implemented"
  else
    Current.component "analyse"
    |> let> repos = Current.list_seq repos in
       Analyse_cache.get () { repos; packages }
