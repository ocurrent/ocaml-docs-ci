let network = [ "host" ]

let download_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache"

let dune_cache = Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

let cache = [ download_cache; dune_cache ]

type mode = Prep | Do

type t = Current_git.Commit.t

module Git = Current_git

let v =
  let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) () in
  Git.clone ~schedule:daily ~gref:"main" "git://github.com/TheLortex/voodoo"

let remote_uri commit =
  let repo = Git.Commit_id.repo commit in
  let commit = Git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let spec ~base mode t =
  let open Obuilder_spec in
  base
  |> Spec.add
       ( match mode with
       | Prep ->
           [
             run ~network "sudo apt-get update && sudo apt-get install -yy m4 pkg-config";
             run ~network ~cache "opam pin -ny %s  && opam depext -iy voodoo-prep"
               (t |> Git.Commit.id |> remote_uri);
             run "cp $(opam config var bin)/voodoo-prep /home/opam";
           ]
       | Do ->
           [
             run ~network "sudo apt-get update && sudo apt-get install -yy m4";
             run ~network
               "opam pin -ny odoc %s && opam depext -iy odoc &&  opam exec -- odoc --version"
               Config.odoc;
             run ~network ~cache "opam pin -ny %s  && opam depext -iy voodoo-do"
               (t |> Git.Commit.id |> remote_uri);
             run "cp $(opam config var bin)/odoc $(opam config var bin)/voodoo-do /home/opam";
           ] )
