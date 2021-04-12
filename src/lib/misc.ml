let base_image_version package =
  let deps = Package.all_deps package in
  let ocaml_version =
    deps
    |> List.find_opt (fun pkg ->
           pkg |> Package.opam |> OpamPackage.name_to_string = "ocaml-base-compiler")
    |> Option.map (fun pkg -> pkg |> Package.opam |> OpamPackage.version_to_string)
    |> Option.value ~default:"4.12.0"
  in
  match Astring.String.cuts ~sep:"." ocaml_version with
  | [ major; minor; _micro ] -> major ^ "." ^ minor
  | _xs -> "4.12"

let get_base_image package = Spec.make ("ocaml/opam:ubuntu-ocaml-" ^ base_image_version package)

let network = [ "host" ]

let docs_cache_folder = "/home/opam/docs-cache/"

let cache = [ Obuilder_spec.Cache.v ~target:docs_cache_folder "ci-docs" ]

let rsync_pull ?(digest = "") folders =
  let sources =
    List.map
      (fun folder -> Fmt.str "%s:%s/./%a" Config.ssh_host Config.storage_folder Fpath.pp folder)
      folders
    |> String.concat " "
  in
  let cache_sources =
    List.map (Fmt.str "%s./%a" docs_cache_folder Fpath.pp) folders |> String.concat " "
  in
  match folders with
  | [] -> Obuilder_spec.comment "no sources to pull"
  | _ ->
      Obuilder_spec.run ~secrets:Config.ssh_secrets ~cache ~network
        "rsync --delete -avzR %s %s  && rsync -aR %s ./ && echo 'pulled: %s'" sources
        docs_cache_folder cache_sources digest
