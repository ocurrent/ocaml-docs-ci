module Git = Current_git

let track = Track.track_packages

let solve = Solver.v

let select_jobs ~targets =
  let open Current.Syntax in
  let+ targets = targets in
  Jobs.schedule ~targets

module Prep = Prep

(*
module Compiled = Git.Commit_id

let compile prep blessed_packages =
  let open Current.Syntax in
  let base =
    let+ bp = blessed_packages in
    get_base_image (List.hd bp)
  in
  let packages =
    let+ blessed = blessed_packages in
    List.map Package.opam blessed
  in
  Compiler.v ~base prep packages

module Assemble = Unit

let assemble_and_link =
  let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
  Assembler.v ~base:(Current.return base)
*)
