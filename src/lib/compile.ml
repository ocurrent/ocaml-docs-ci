type t = { package : Package.t; blessed : bool; odoc : Mld.Gen.odoc_dyn }

let is_blessed t = t.blessed

let odoc t = t.odoc

let package t = t.package

let network = Voodoo.network

let folder ~blessed package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fpath.(v "compile" / "packages" / name / version)
  else Fpath.(v "compile" / "universes" / universe / name / version)

let import_deps t =
  let folders = List.map (fun { package; blessed; _ } -> folder ~blessed package) t in
  Misc.rsync_pull folders

let spec ~base ~deps ~blessed prep =
  let open Obuilder_spec in
  let prep_folder = Prep.folder prep in
  let package = Prep.package prep in
  let compile_folder = folder ~blessed package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let version = opam |> OpamPackage.version_to_string in

  let odoc_package =
    Mld.{ file = Fpath.(compile_folder / "the_page.mld"); target = None; name = "the_page"; kind = Mld }
  in
  let odoc_version_page =
    Mld.
      {
        file = Fpath.(v "compile" / "packages" / name / (name_of_string version ^ ".mld"));
        target = None;
        name = name_of_string version;
        kind = Mld;
      }
  in
  let odoc_versions_index =
    Mld.
      {
        file = Fpath.(v "compile" / "packages" / (name_of_string name ^ ".mld"));
        target = None;
        name = name_of_string name;
        kind = Mld;
      }
  in
  let odoc_packages_index =
    Mld.
      { file = Fpath.(v "compile" / "packages.mld"); target = None; name = "packages"; kind = Mld }
  in
  (* let package = Prep.package target in
     let package_name = OpamPackage.name (Package.opam package) |> OpamPackage.Name.to_string in
      let is_blessed = Package.Blessed.is_blessed blessed package in*)
  ( base
    |> Spec.add
         [
           run ~network "opam pin -ny odoc %s && opam depext -iy odoc" Config.odoc;
           workdir "/home/opam/docs/";
           run "sudo chown opam:opam .";
           import_deps deps;
           Misc.rsync_pull [ prep_folder ];
           run "find . -type d";
           run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
           run "%s"
           @@ Fmt.str
                {|
          eval $(opam config env)
          echo '{0 Package root page}' >> %a
          touch %a && touch %a && touch %a
          %a # compile fake packages index 
          %a # compile fake versions index
          %a # compile fake versions page
          %a # compile package
          %a # link package
          %a # html package
         |}
                Fpath.pp odoc_package.file Fpath.pp odoc_versions_index.file Fpath.pp
                odoc_version_page.file Fpath.pp odoc_packages_index.file Mld.pp_compile_command
                (Mld.v ~children:[ odoc_versions_index ] odoc_packages_index)
                Mld.pp_compile_command
                (Mld.v ~children:[ odoc_version_page ] ~parent:odoc_packages_index
                   odoc_versions_index)
                Mld.pp_compile_command
                (Mld.v ~children:[ odoc_package ] ~parent:odoc_versions_index odoc_version_page)
                Mld.pp_compile_command
                (Mld.v ~parent:odoc_version_page odoc_package)
                Mld.pp_link_command
                (Mld.v ~parent:odoc_versions_index odoc_package)
                (Mld.pp_html_command ~output:(Fpath.v "/home/opam/html") ())
                odoc_package;
           workdir "/home/opam/docs/compile";
           run "rm packages.mld page-packages.odoc packages/*.mld packages/*.odoc";
           run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./compile/ %s:%s/"
             Config.ssh_host Config.storage_folder;
           run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/./html/ %s:%s/"
             Config.ssh_host Config.storage_folder;
         ],
    odoc_package )

let folder { package; blessed; _ } = folder ~blessed package

let v ~blessed ~deps target =
  let open Current.Syntax in
  let spec =
    let+ deps = deps and+ prep = target and+ blessed = blessed in
    let package = Prep.package prep in
    let blessed = Package.Blessed.is_blessed blessed package in
    let spec, odoc = spec ~base:(Misc.get_base_image (Prep.package prep)) ~deps ~blessed prep in
    (spec |> Spec.to_ocluster_spec, odoc)
  in
  let odoc = Current.map snd spec in
  let spec = Current.map fst spec in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    let* target = target in
    Current_ocluster.build_obuilder
      ~label:(Fmt.str "odoc\n%s" (Prep.package target |> Package.opam |> OpamPackage.to_string))
      ~src:(Current.return []) ~pool:Config.pool ~cache_hint:"docs-universe-build" cluster spec
  and+ odoc = odoc
  and+ prep = target
  and+ blessed = blessed in
  let package = Prep.package prep in
  let blessed = Package.Blessed.is_blessed blessed package in
  { package; blessed; odoc = Mld odoc }
