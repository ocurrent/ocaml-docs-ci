type t = Package.t

let network = Voodoo.network

let folder ~blessed t =
  let universe = Package.universe t |> Package.Universe.hash in
  let opam = Package.opam t in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if Package.Blessed.is_blessed blessed t then Fmt.str "/compile/packages/%s/%s/" name version
  else Fmt.str "/compile/universes/%s/%s/%s" universe name version

let cache = [ Obuilder_spec.Cache.v ~target:"/home/.opam/docs/" "ci-docs" ]

let import_dep ~blessed dep =
  let folder = folder ~blessed dep in
  Obuilder_spec.run ~secrets:Config.ssh_secrets ~cache ~network
    "rsync -avzR %s:%s/./%s /home/opam/docs/" Config.ssh_host Config.storage_folder folder 

let spec ~base ~blessed ~deps target =
  let open Obuilder_spec in
  let prep_folder = Prep.folder target in
  let compile_folder = folder ~blessed (Prep.package target) in
  let package = Prep.package target in
  let package_name = OpamPackage.name (Package.opam package) |> OpamPackage.Name.to_string in
  let is_blessed = Package.Blessed.is_blessed blessed package in
  Voodoo.spec ~base ~prep:true ~link:true
  |> Spec.add
       ( List.map (import_dep ~blessed) deps
       @ [
           run ~secrets:Config.ssh_secrets ~cache ~network
             "rsync -avzR %s:%s/./%s /home/opam/docs/" Config.ssh_host Config.storage_folder prep_folder;
           run "~/voodoo-link compile --package %s --blessed %b" package_name is_blessed;
           run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./%s %s:%s/"
             compile_folder Config.ssh_host Config.storage_folder;
         ] )

let v ~blessed ~deps target =
  let open Current.Syntax in
  let spec =
    let+ deps = deps and+ blessed = blessed and+ target = target in
    spec ~base:(Misc.get_base_image (Prep.package target)) ~blessed ~deps target
    |> Spec.to_ocluster_spec
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster =
    Current_ocluster.v ~secrets:Config.ssh_secrets_values conn
  in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:(Current.return [])
      ~pool:"linux-arm64" ~cache_hint:"docs-universe-build" cluster spec
  and+ target = target in
  Prep.package target
