let opam_download_cache =
  Obuilder_spec.Cache.v "opam-download-cache" ~target:"/home/opam/.opam/download-cache"

let network = [ "host" ]

let add_repositories =
  List.map (fun (name, repo) -> Obuilder_spec.run ~network "opam repo add %s %s" name repo)

let install_tools tools =
  let tools_s = String.concat " " tools in
  [
    Obuilder_spec.run ~network ~cache:[ opam_download_cache ] "opam depext %s" tools_s;
    Obuilder_spec.run ~network ~cache:[ opam_download_cache ] "opam install %s" tools_s;
  ]
