let network = [ "host" ]

let download_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache"

let build_cache =
  Obuilder_spec.Cache.v "opam-build-cache" ~target:"/home/opam/.cache/opam-bin-cache"

let dune_cache = Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

let cache = [ download_cache; build_cache; dune_cache ]

let build_cache_config =
  {| 
pre-install-commands:
  ["%{hooks}%/opam-bin-cache.sh" "restore" build-id name] {?build-id}
wrap-build-commands: [
  ["%{hooks}%/opam-bin-cache.sh" "wrap" build-id] {?build-id}
  ["%{hooks}%/sandbox.sh" "build"] {os = "linux"}
]
wrap-install-commands: [
  ["%{hooks}%/opam-bin-cache.sh" "wrap" build-id] {?build-id}
  ["%{hooks}%/sandbox.sh" "install"] {os = "linux"}
]
wrap-remove-commands: ["%{hooks}%/sandbox.sh" "remove"] {os = "linux"}
post-install-commands:
  ["%{hooks}%/opam-bin-cache.sh" "store" build-id installed-files]
    {?build-id & error-code = "0"}
|}

let spec ~base ~prep ~link =
  let open Obuilder_spec in
  let pkgs, pins, cps =
    if prep then
      ( [ "voodoo-prep" ],
        [ run ~network ~cache "opam pin add -ny git://github.com/jonludlam/voodoo-prep" ],
        [ run "cp $(opam config var bin)/voodoo_prep /home/opam" ] )
    else ([], [], [])
  in
  let pkgs, pins, cps =
    if link then
      ( "voodoo" :: "voodoo_lib" :: pkgs,
        run ~network ~cache "opam pin add -ny https://github.com/TheLortex/voodoo.git#main  " :: pins,
        run "cp $(opam config var bin)/voodoo-link /home/opam" :: cps )
    else (pkgs, pins, cps)
  in

  base
  |> Spec.add
       ( [
           run ~network "sudo apt-get update && sudo apt-get install -yy m4";
           (* Enable binary cache *)
           run ~network
             "curl https://raw.githubusercontent.com/ocaml/opam/2.0.8/shell/opam-bin-cache.sh -O \
              && chmod +x opam-bin-cache.sh && mv opam-bin-cache.sh \
              /home/opam/.opam/opam-init/hooks/";
           run "cat /home/opam/.opam/config";
           (*  run "for i in {1..3}; do sed -i '$d' /home/opam/.opam/config; done; ";*)
           run "echo '%s' >> /home/opam/.opam/config" build_cache_config;
           run "cat /home/opam/.opam/config";
           (* Update opam *)
           env "OPAMPRECISETRACKING" "1";
           (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
           env "OPAMDEPEXTYES" "1";
         ]
       @ pins
       @ [ run ~network ~cache "opam depext -yi %s" (String.concat " " pkgs) ]
       @ cps
       @ [ run "opam remove -ay %s" (String.concat " " pkgs) ] )
