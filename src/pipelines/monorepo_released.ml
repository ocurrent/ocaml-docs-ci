module Git = Current_git
module Docker = Current_docker.Default
open Current.Syntax
open Mirage_ci_lib

let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

let v ~repos (projects : Universe.Project.t list) =
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
      let lock = Monorepo.lock ~base ~opam:(Current.return configuration) in
      let base_freestanding =
        (* add ocaml-freestanding *)
        let+ base = base in
        Spec.add (Setup.install_tools [ "ocaml-freestanding"; "ocamlfind.1.8.1" ]) base
      in
      let spec = Monorepo.monorepo_released ~base ~lock () in
      let spec_freestanding = Monorepo.monorepo_released ~base:base_freestanding ~lock () in
      let dune_build =
        let+ spec = spec in
        Spec.add [ Obuilder_spec.run "opam exec -- dune build @install" ] spec
      in
      let dune_build_freestanding =
        let+ spec = spec_freestanding in
        Spec.add [ Obuilder_spec.run "opam exec -- dune build @install -x freestanding" ] spec
      in
      let cache_hint = "mirage-ci-monorepo-released" in
      let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
      [
        Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64"
          ~src:(Current.return [])
          (dune_build |> Config.to_ocluster_spec);
        Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64"
          ~src:(Current.return [])
          (dune_build_freestanding |> Config.to_ocluster_spec);
      ]
      |> Current.all)
