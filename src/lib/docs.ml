module Git = Current_git

let track = Track.track_packages

let solve = Solver.v ?universe:None

let explode = Solver.explode

let bless_packages = Current.map Package.bless

type job = Jobs.t

let select_jobs ~targets ~blessed =
  let open Current.Syntax in
  let+ targets = targets and+ blessed = blessed in
  Jobs.schedule ~targets ~blessed

module Prep = Git.Commit_id
(** The branch in which the prep has been pushed *)

let get_base_image package =
  let deps = Package.universe package |> Universe.deps in
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

let build_and_prep package =
  let open Current.Syntax in
  let pkg =
    let+ package = package in
    Package.opam package
  in 
  let commit =
    let+ package = package in 
    Package.commit package 
  in
  let deps =
    let+ package = package in
    Package.universe package |> Universe.deps
  in
  let base =
    let+ package = package in
    get_base_image package
  in
  Builder.v ~commit ~base pkg deps

module Linked = Git.Commit_id

let link prep blessed_packages =
  let open Current.Syntax in
  let base =
    let+ bp = blessed_packages in
    get_base_image (List.hd bp)
  in
  let packages =
    let+ blessed = blessed_packages in
    List.map Package.opam blessed
  in
  Linker.v ~base prep packages

module Assemble = Unit

let assemble_and_link =
  let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
  Assembler.v ~base:(Current.return base)
