module Docker = Current_docker.Default
open Current.Syntax
open Mirage_ci_lib

let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

let v ~repos packages =
  let base =
    let+ repos = Current.list_seq repos in
    let repos = List.map (fun (name, commit) -> (name, remote_uri commit)) repos in
    Spec.make "ocaml/opam:ubuntu-ocaml-4.11" |> Spec.add (Setup.add_repositories repos)
  in
  let opam = Current.return (Monorepo.opam_file ~ocaml_version:"4.11.1" packages) in
  let lock = Monorepo.lock ~base ~opam in
  let base_freestanding =
    (* add ocaml-freestanding *)
    let+ base = base in
    Spec.add (Setup.install_tools [ "ocaml-freestanding"; "ocamlfind.1.8.1" ]) base
  in
  let spec = Monorepo.monorepo_main ~base ~lock () in
  let spec_freestanding = Monorepo.monorepo_main ~base:base_freestanding ~lock () in
  let dune_build =
    let+ spec = spec in
    Spec.add [ Obuilder_spec.run "opam exec -- dune build @install" ] spec
  in
  let dune_build_freestanding =
    let+ spec = spec_freestanding in
    Spec.add [ Obuilder_spec.run "opam exec -- dune build @install -x freestanding" ] spec
  in
  let cache_hint = "mirage-ci-monorepo-edge" in
  let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
  [
    Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64" ~src:(Current.return [])
      (dune_build |> Config.to_ocluster_spec);
    Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64" ~src:(Current.return [])
      (dune_build_freestanding |> Config.to_ocluster_spec);
  ]
  |> Current.all
