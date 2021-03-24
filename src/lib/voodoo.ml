let network = [ "host" ]

let download_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache"

let dune_cache = Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

let cache = [ download_cache; dune_cache ]

type mode = Prep | Do

let spec ~base mode =
  let open Obuilder_spec in
  let pin_install_voodoo =
    run ~network ~cache
      "opam pin -ny git://github.com/TheLortex/voodoo  && opam depext -iy voodoo-lib "
  in
  let pin_install_odoc =
    run ~network ~cache "opam pin -ny odoc %s && opam depext -iy odoc" Config.odoc
  in
  let pkg =
    match mode with
    | Prep -> "voodoo-prep "
    | Do -> "voodoo-do"
  in
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
           run "opam remove -ay odoc voodoo-lib %s" pkg;
           run "cp /home/opam/odoc /home/opam/%s $(opam config var bin)" pkg;
         ] )
