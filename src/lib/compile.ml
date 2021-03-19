type t = { package : Package.t; blessed : bool }

let is_blessed t = t.blessed

let package t = t.package

let network = Voodoo.network

let folder { package; blessed } =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fmt.str "/compile/packages/%s/%s/" name version
  else Fmt.str "/compile/universes/%s/%s/%s" universe name version

let cache = [ Obuilder_spec.Cache.v ~target:"/home/opam/docs-cache/" "ci-docs" ]

let import_deps t =
  let folders = List.map folder t in
  let sources =
    List.map
      (fun folder -> Fmt.str "%s:%s/./%s" Config.ssh_host Config.storage_folder folder)
      folders
    |> String.concat " "
  in
  let cache_sources =
    List.map (Fmt.str "/home/opam/docs-cache/./%s") folders |> String.concat " "
  in
  match t with
  | [] -> Obuilder_spec.comment "no deps to import"
  | _ ->
      Obuilder_spec.run ~secrets:Config.ssh_secrets ~cache ~network
        "rsync -avzR %s /home/opam/docs-cache/ && rsync -aR %s /home/opam/docs/ " sources
        cache_sources

let spec ~base ~deps t prep =
  let open Obuilder_spec in
  let prep_folder = Prep.folder prep in
  let compile_folder = folder t in
  (* let package = Prep.package target in
     let package_name = OpamPackage.name (Package.opam package) |> OpamPackage.Name.to_string in
      let is_blessed = Package.Blessed.is_blessed blessed package in*)
  Voodoo.spec ~base ~prep:true ~link:true
  |> Spec.add
       [
         import_deps deps;
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR %s:%s/./%s /home/opam/docs/"
           Config.ssh_host Config.storage_folder prep_folder;
         run "sudo chown opam:opam /home/opam/docs/";
         run "find /home/opam/docs/ -type d";
         run "mkdir -p /home/opam/docs/%s" compile_folder;
         run "mv /home/opam/docs/%s  /home/opam/docs/%s" prep_folder compile_folder;
         workdir ("/home/opam/docs/" ^ compile_folder);
         run "find . -type f -name '*.cmi' -exec sh -c 'mv \"$1\" \"${1%%.cmi}.odoc\"' _ {} \\;";
         run "find . -type f -name '*.cmti' -exec sh -c 'mv \"$1\" \"${1%%.cmti}.odoc\"' _ {} \\;";
         run "find . -type f -name '*.cmt' -exec sh -c 'mv \"$1\" \"${1%%.cmt}.odoc\"' _ {} \\;";
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./%s %s:%s/"
           compile_folder Config.ssh_host Config.storage_folder;
       ]

let v ~blessed ~deps target =
  let open Current.Syntax in
  let t =
    let+ blessed = blessed and+ target = target in
    let package = Prep.package target in
    { package; blessed = Package.Blessed.is_blessed blessed package }
  in
  let spec =
    let+ deps = deps and+ prep = target and+ t = t in
    spec ~base:(Misc.get_base_image (Prep.package prep)) ~deps t prep |> Spec.to_ocluster_spec
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:(Current.return [])
      ~pool:"linux-arm64" ~cache_hint:"docs-universe-build" cluster spec
  and+ t = t in
  t
