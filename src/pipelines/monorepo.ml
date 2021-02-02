module Git = Current_git
module Docker = Current_docker.Default
open Current.Syntax
open Mirage_ci_lib

let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

type mode = Edge | Released

type toolchain = Host | Freestanding

let pp_toolchain () = function Host -> "" | Freestanding -> "-x freestanding"

let v ~roots ~mode ?(toolchain = Host) ~repos ~lock () =
  let repos =
    let+ repos = repos in
    List.map (fun (name, commit) -> (name, remote_uri commit)) repos
  in
  let base =
    let+ repos = repos in
    Spec.make "ocaml/opam:ubuntu-ocaml-4.11" |> Spec.add (Setup.add_repositories repos)
  in
  let base =
    let+ base = base in
    match toolchain with
    | Host -> base
    | Freestanding ->
        Spec.add (Setup.install_tools [ "ocaml-freestanding"; "ocamlfind.1.8.1" ]) base
  in
  let monorepo_builder =
    match mode with Edge -> Monorepo.monorepo_main | Released -> Monorepo.monorepo_released
  in
  let spec = monorepo_builder ~roots ~base ~lock () in
  let dune_build =
    let+ spec = spec in
    Spec.add
      [ Obuilder_spec.run "opam exec -- dune build --profile release %a" pp_toolchain toolchain ]
      spec
  in
  let cache_hint = "mirage-ci-monorepo" in
  let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
  Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64" ~src:(Current.return [])
    (dune_build |> Config.to_ocluster_spec)

let lock ~repos (projects : Universe.Project.t list) =
  let repos = Current.list_seq repos in
  Current.with_context repos (fun () ->
      let repos =
        let+ repos = repos in
        List.map (fun (name, commit) -> (name, remote_uri commit)) repos
      in
      let base =
        let+ repos = repos in
        Spec.make "ocaml/opam:ubuntu-ocaml-4.11" |> Spec.add (Setup.add_repositories repos)
      in
      let configuration = Monorepo.opam_file ~ocaml_version:"4.11.1" projects in
      Monorepo.lock ~base ~opam:(Current.return configuration))

let edge ~roots ~repos ~lock =
  [
    ("edge-freestanding", v ~roots ~mode:Edge ~toolchain:Freestanding ~repos ~lock ());
    ("edge-host", v ~roots ~mode:Edge ~repos ~lock ());
  ]
  |> Current.all_labelled

let released ~roots ~repos ~lock =
  [
    ("released-freestanding", v ~roots ~mode:Released ~toolchain:Freestanding ~repos ~lock ());
    ("released-host", v ~roots ~mode:Released ~repos ~lock ());
  ]
  |> Current.all_labelled
