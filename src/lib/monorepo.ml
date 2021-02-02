open Lwt.Infix
open Current.Syntax

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

let pool = Current.Pool.create ~label:"monorepo-pool" 4

let parse_opam_dev_repo dev_repo =
  let module String = Astring.String in
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

let fetch_rule (_, commit) =
  let id = Current_git.Commit.id commit in
  let clone_cmd = Fmt.str "%a" Current_git.Commit_id.pp_user_clone id in
  Obuilder_spec.run ~network:Setup.network "%s" clone_cmd

let get_alias =
  let pp_alias f (_, commit) =
    let repo = commit |> Current_git.Commit.id |> Current_git.Commit_id.repo in
    let name =
      match Filename.basename repo |> Filename.remove_extension with "dune" -> "dune_" | ok -> ok
    in
    Fmt.pf f "(alias_rec %s/install)\n" name
  in
  Fmt.str {|
  (alias
  (name default)
  (deps %a)
  )
  |} (Fmt.list pp_alias)

let filter_roots ~(roots : Universe.Project.t list) (projects : (string * _) list) =
  projects
  |> List.filter (fun (name, _) ->
         List.exists (fun { Universe.Project.opam; _ } -> List.mem name opam) roots)

let monorepo_main ~roots ~base ~lock () =
  let+ projects =
    let* lock = lock in
    (* Bind: the list of tracked projects is dynamic *)
    let projects = Monorepo_lock.projects lock in
    Printf.printf "got %d projects to track.\n" (List.length projects);
    List.map
      (fun (x : Monorepo_lock.project) ->
        let repo_url, repo_branch = parse_opam_dev_repo x.dev_repo in
        let+ commit = Current_git.clone ~schedule:daily ?gref:repo_branch repo_url in
        (x.name, commit))
      projects
    |> Current.list_seq
  and+ base = base
  and+ lock = lock in
  let lockfile = Monorepo_lock.lockfile lock in
  let open Obuilder_spec in
  base
  |> Spec.add (Setup.install_tools [ "dune"; "opam-monorepo" ])
  |> Spec.add
       [
         user ~uid:1000 ~gid:1000;
         workdir "/src";
         run "sudo chown opam:opam /src";
         (* External dependencies *)
         run "echo '%s' >> monorepo.opam" (Opamfile.marshal lockfile);
         run "opam pin -n add monorepo . --locked --ignore-pin-depends";
         run "opam depext --update -y monorepo";
         run "opam pin -n remove monorepo";
       ]
  (* assemble monorepo sequentially *)
  |> Spec.add (List.map fetch_rule projects)
  |> Spec.add
       [
         run "touch dune && mv dune dune_";
         run "echo '%s' >> dune" (get_alias (projects |> filter_roots ~roots));
       ]

(********************************************)
(***************   RELEASED   ***************)
(********************************************)

let filter_roots ~(roots : Universe.Project.t list) (projects : Monorepo_lock.project list) =
  projects
  |> List.filter (fun { Monorepo_lock.name; _ } ->
         List.exists (fun { Universe.Project.opam; _ } -> List.mem name opam) roots)

let get_alias =
  let repo_name t =
    let uri = Uri.of_string t in
    let path = Uri.path uri in
    let last_path_component =
      match Astring.String.cut ~rev:true ~sep:"/" path with
      | None -> path
      | Some (_, last_path_component) -> last_path_component
    in
    match Astring.String.cut ~sep:"." last_path_component with
    | None -> last_path_component
    | Some (repo_name, _ext) -> repo_name
  in
  let pp_alias f (project : Monorepo_lock.project) =
    let name = match repo_name project.dev_repo with "dune" -> "dune_" | ok -> ok in
    Fmt.pf f "(alias_rec duniverse/%s/install)\n" name
  in
  Fmt.str {|
  (alias
   (name default)
   (deps %a)
  )
  |} (Fmt.list pp_alias)

let monorepo_released ~roots ~base ~lock () =
  let+ lock = lock and+ base = base in
  let projects = Monorepo_lock.projects lock in
  let opamfile = Monorepo_lock.lockfile lock in
  let open Obuilder_spec in
  base
  |> Spec.add (Setup.install_tools [ "dune"; "opam-monorepo" ])
  |> Spec.add
       [
         user ~uid:1000 ~gid:1000;
         workdir "/src";
         run "sudo chown opam:opam /src";
         run "echo '%s' >> monorepo.opam.locked" (Opamfile.marshal opamfile);
         run "echo '(name monorepo)' >> dune-project";
         (* opam monorepo uses the dune project to find which lockfile to pull*)
         run ~network:Setup.network "opam exec -- opam monorepo pull -y";
         run "rm duniverse/dune" (* removed the vendored mark to allow the build *);
         run "echo '%s' >> dune" (get_alias (projects |> filter_roots ~roots)) (* create alias *);
       ]

(********************************************)
(*****************  LOCK  *******************)
(********************************************)

let ( let>> ) = Lwt_result.bind

module Lock = struct
  type t = unit

  module Key = struct
    type t = { base : Spec.t; opam : Opamfile.t }

    let digest { base; opam } =
      let json =
        `Assoc [ ("spec", Spec.to_json base); ("opam", `String (Opamfile.marshal opam)) ]
      in
      Yojson.to_string json
  end

  module Value = Monorepo_lock

  let id = "mirage-ci-monorepo-lock"

  let generate_monorepo =
    let open Obuilder_spec in
    [
      workdir "/src/";
      run "sudo chown opam /src";
      copy [ "monorepo.opam" ] ~dst:"/src/";
      run "opam monorepo lock";
    ]

  let build () job { Key.base; opam } =
    let switch = Current.Switch.create ~label:"monorepo-lock-switch" () in
    Lwt.finalize
      (fun () ->
        Current.Job.use_pool ~switch job pool >>= fun () ->
        Current.Job.start ~level:Harmless job >>= fun () ->
        let spec =
          base
          |> Spec.add (Setup.install_tools [ "opam-monorepo" ])
          |> Spec.add generate_monorepo |> Spec.finish
        in
        let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true spec in
        Current.Job.log job "Starting docker build to generate lockfile.";
        let>> id =
          Current.Process.with_tmpdir (fun tmpdir ->
              Bos.OS.File.write Fpath.(tmpdir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
              Bos.OS.File.write Fpath.(tmpdir / "monorepo.opam") (Opamfile.marshal opam) |> or_raise;
              Current.Job.log job "----\nUsing opam file:\n%s\n----" (Opamfile.marshal opam);
              let iidfile = Fpath.(tmpdir / "iidfile") in
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
        Lwt.return_ok
          (Monorepo_lock.make ~opam_file:lockfile
             ~dev_repo_output:(String.split_on_char '\n' dev_repos_str)))
      (fun () -> Current.Switch.turn_off switch)

  let pp f _ = Fmt.string f "opam-monorepo lock"

  let auto_cancel = true
end

module Lock_cache = Current_cache.Make (Lock)

let lock ~base ~opam =
  Current.component "opam-monorepo lock"
  |> let> base = base and> opam = opam in
     Lock_cache.get () { base; opam }

let opam_file ~ocaml_version (projects : Universe.Project.t list) =
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
  |> Opamfile.unmarshal
