open Current.Syntax

let pool = Current.Pool.create ~label:"monorepo-pool" 4


module Docker = Current_docker.Default

(********************************************)
(*****************  LOCK  *******************)
(********************************************)

type t = Docker.Image.t

let v ~repos =
  Current_solver.v ~repos ~packages:["opam-monorepo"]
  |> Setup.tools_image ~name:"opam-monorepo tool"

let add_repos repos =
  let remote_uri commit =
    let commit_id = Current_git.Commit.id commit in
    let repo = Current_git.Commit_id.repo commit_id in
    let commit = Current_git.Commit.hash commit in
    repo ^ "#" ^ commit
  in
  let open Dockerfile in
  let repo_add (name, commit) = run "opam repo add %s %s" name (remote_uri commit)  in
  List.fold_left (@@) (run "opam repo remove local") (List.map repo_add repos) 


let pp_wrap =
  Fmt.using (String.split_on_char '\n')
    Fmt.(list ~sep:(unit " \\@\n    ") (using String.trim string))
  
let lock ~repos ~opam t =
  let dockerfile =
    let+ t = t 
    and+ opam = opam 
    and+ repos = repos 
    in
    let open Dockerfile in
    from (Docker.Image.hash t)
    @@ user "opam"
    @@ copy ~chown:"opam" ~src:[ "." ] ~dst:"/src" ()
    @@ workdir "/src"
    @@ run "echo '%s' >> monorepo.opam" (Fmt.str "%a" pp_wrap (Opamfile.marshal opam))
    @@ add_repos repos
    @@ run "opam monorepo lock"
    |> fun dockerfile -> `Contents dockerfile
  in
  let image =
    Docker.build ~dockerfile
      ~label:("opam monorepo lock")
      ~pool ~pull:false (`No_context)
  in   
  Current.component "monorepo lockfile" |>     
  let** lockfile_str = Docker.pread ~label:"lockfile" ~args:["cat"; "/src/monorepo.opam.locked"] image in
  let lockfile = OpamParser.string lockfile_str "monorepo.opam.locked" in
  let packages = Opamfile.get_packages lockfile in
  let+ dev_repos_str = Docker.pread ~label:"dev repos" ~args:(["opam"; "show"; "--field"; "name:,dev-repo:"]
    @ List.map (fun (pkg : Opamfile.pkg) -> pkg.name ^ "." ^ pkg.version) packages ) image in
  Monorepo_lock.make ~opam_file:lockfile
  ~dev_repo_output:(String.split_on_char '\n' dev_repos_str)

  let lock ~value ~repos ~opam t =
    Current.collapse ~key:"monorepo-lock" ~value ~input:opam (lock ~repos ~opam t)

(********************************************)
(*****************  EDGE  *******************)
(********************************************)

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

let monorepo_main ?(name = "main") ~base ~lock () =
  let+ projects =
    Current.component "track projects from lockfile" |>
    let** lockv = lock in
    (* Bind: the list of tracked projects is dynamic *)
    let projects = Monorepo_lock.projects lockv in
    Printf.printf "got %d projects to track.\n" (List.length projects);
    List.map
      (fun (x : Monorepo_lock.project) ->
        let repo_url, repo_branch = parse_opam_dev_repo x.dev_repo in
        let+ commit = Current_git.clone ~schedule:daily ?gref:repo_branch repo_url in
        (x.name, commit))
      projects
    |> Current.list_seq
    |> Current.collapse ~key:"monorepo-main" ~value:name ~input:lock
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
         run ~network:Setup.network "opam depext --update -y monorepo";
         run "opam pin -n remove monorepo";
       ]
  (* assemble monorepo sequentially *)
  |> Spec.add (
    (workdir "/src/duniverse")::
    (run "sudo chown opam:opam /src/duniverse")::(List.map fetch_rule projects))
  (* rename dune to dune_ to mimic opam-monorepo behavior *)
  |> Spec.add [
    run "touch dune && mv dune dune_";
    run "echo '(vendored_dirs *)' >> dune";
    workdir "/src"
  ]

(********************************************)
(***************   RELEASED   ***************)
(********************************************)

let monorepo_released ~base ~lock () =
  let+ lock = lock and+ base = base in
  let opamfile = Monorepo_lock.lockfile lock in
  let open Obuilder_spec in
  base
  |> Spec.add (Setup.install_tools [ "dune"; "opam-monorepo" ])
  |> Spec.add
       [
         user ~uid:1000 ~gid:1000;
         workdir "/src";
         run "sudo chown opam:opam /src";
         run "echo '%s' >> monorepo.opam" (Opamfile.marshal opamfile);
         (* depexts  *)
         run "opam pin -n add monorepo . --locked --ignore-pin-depends";
         run ~network:Setup.network "opam depext --update -y monorepo";
         run "opam pin -n remove monorepo";
         (* setup lockfile *)
         run "cp monorepo.opam monorepo.opam.locked";
         run "echo '(name monorepo)' >> dune-project";
         (* opam monorepo uses the dune project to find which lockfile to pull*)
         run ~network:Setup.network "opam exec -- opam monorepo pull -y";
       ]

(********************************************)
(********************************************)
(********************************************)

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
