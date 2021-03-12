module Git = Current_git

let track = Track.track_packages

let solve = Solver.v

let explode = Solver.explode

let bless_packages = Current.map Package.bless

let get_jobs ~targets:_ = failwith "unimplemented"

module Prep = Git.Commit_id
(** The branch in which the prep has been pushed *)

let build_and_prep ~opam package =
  let open Current.Syntax in
  let pkg =
    let+ package = package in
    Package.opam package
  in
  let deps =
    let+ package = package in
    Package.universe package |> Universe.deps
  in
  let base =
    let+ deps = deps in
    let ocaml_version =
      deps
      |> List.find_opt (fun pkg -> OpamPackage.name_to_string pkg = "ocaml")
      |> Option.map OpamPackage.version_to_string
      |> Option.value ~default:"4.12.0"
    in
    let base_image_version =
      match Astring.String.cuts ~sep:"." ocaml_version with
      | [ major; minor; _micro ] ->
          Format.eprintf "major: %s minor: %s\n%!" major minor;
          major ^ "." ^ minor
      | _xs -> "4.12"
    in
    Spec.make ("ocaml/opam:ubuntu-ocaml-" ^ base_image_version)
  in
  Builder.v ~opam ~base pkg deps

module Assemble = Unit

let assemble_and_link =
  let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
  Assembler.v ~base:(Current.return base)
