let network = [ "host" ]

let download_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache"

let dune_cache = Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

let cache = [ download_cache; dune_cache ]

type mode = Prep | Do

type t = Current_git.Commit.t

module Git = Current_git

let v =
  let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) () in
  Git.clone ~schedule:daily ~gref:"main" "git://github.com/jonludlam/voodoo"

let remote_uri commit =
  let repo = Git.Commit_id.repo commit in
  let commit = Git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let spec ~base mode t =
  let open Obuilder_spec in
  let pin_install_voodoo =
    run ~network ~cache "opam pin -ny %s  && opam depext -iy voodoo-lib"
      (t |> Git.Commit.id |> remote_uri)
  in
  let pin_install_odoc =
    run ~network "opam pin -ny odoc %s && opam depext -iy odoc &&  opam exec -- odoc --version"
      Config.odoc
  in
  let pkg = match mode with Prep -> "voodoo-prep" | Do -> "voodoo-do" in
  base
  |> Spec.add
       ( [
           run ~network "sudo apt-get update && sudo apt-get install -yy m4";
           (* Update opam *)
           env "OPAMPRECISETRACKING" "1";
           (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
           env "OPAMDEPEXTYES" "1";
         ]
       @ [
           pin_install_odoc;
           pin_install_voodoo;
           run ~network ~cache "opam depext -yi %s" pkg;
           run "cp $(opam config var bin)/odoc $(opam config var bin)/%s /home/opam" pkg;
         ] )
