type t = unit

let network = Voodoo.network

let cache = [ Obuilder_spec.Cache.v ~target:"/home/opam/docs-cache/" "ci-docs" ]

let generate_mlds_script f (mld : Mld.t) =
  let packages = mld.packages |> OpamPackage.Name.Map.keys in
  let universes = mld.universes |> Mld.StringMap.bindings |> List.map fst in
  let open Fmt in
  let pp_package_page f package =
    pf f "echo '%a' >> /home/opam/docs/compile/packages/%a.mld" (Mld.package ~t:mld) package
      Mld.pp_name
      (OpamPackage.Name.to_string package)
  in
  let pp_universe_page f universe =
    pf f "echo '%a' >> /home/opam/docs/compile/universes/%a.mld" (Mld.universe ~t:mld) universe
      Mld.pp_name universe
  in
  pf f
    {|
  echo '%a' >> /home/opam/docs/compile/packages.mld
  echo '%a' >> /home/opam/docs/compile/universes.mld
  %a
  %a
  |}
    Mld.packages mld Mld.universes mld
    (list ~sep:(any "\n") pp_package_page)
    packages
    (list ~sep:(any "\n") pp_universe_page)
    universes

let compile_package_page ~(mld : Mld.t) f name =
  let versions = OpamPackage.Name.Map.find name mld.packages in
  let children =
    versions |> OpamPackage.Version.Set.elements |> List.map OpamPackage.Version.to_string
  in
  let pp_child_arg f = Fmt.pf f "--child page-%a" Mld.pp_name in
  Fmt.pf f "odoc compile packages/%a.mld -I . --parent page-packages %a" Mld.pp_name
    (name |> OpamPackage.Name.to_string)
    (Fmt.list ~sep:(Fmt.any " ") pp_child_arg)
    children

let spec ~base (packages : Compile.t list) =
  let mld = Mld.v packages in
  let packages = mld.packages |> OpamPackage.Name.Map.keys in
  (* let universes = mld.universes |> Mld.StringMap.bindings |> List.map fst in *)
  let open Obuilder_spec in
  base
  |> Spec.add
       [
         run ~network "opam pin -ny odoc %s && opam depext -iy odoc" Config.odoc;
         run ~secrets:Config.ssh_secrets ~cache ~network
           "rsync -avzR %s:%s/./compile /home/opam/docs-cache/ && rsync -aR \
            /home/opam/docs-cache/./compile /home/opam/docs/"
           Config.ssh_host Config.storage_folder;
         run "find /home/opam/docs -type d";
         run {|%s|} @@ Fmt.to_to_string generate_mlds_script mld;
         workdir "/home/opam/docs/compile";
         run "%s"
         @@ Fmt.str
              {|
           eval $(opam config env)
           odoc compile packages.mld %s
           %a
           odoc link page-packages.odoc -I packages/
           find packages -maxdepth 1 -type f -name '*.odoc' -exec odoc link {} -I ./packages/$(basename {} .odoc) -I . \;
           find -maxdepth 2 -type f -name '*.odocl' -exec odoc html -o /home/opam/html {} \;
           odoc support-files -o /home/opam/html
           |}
              ( packages
              |> List.map (fun pkg ->
                     Fmt.str "--child page-%a" Mld.pp_name (OpamPackage.Name.to_string pkg))
              |> String.concat " " )
              (Fmt.list ~sep:(Fmt.any "\n") (compile_package_page ~mld))
              packages;
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR --exclude=\"/*/*/\" . %s:%s/test"
           Config.ssh_host Config.storage_folder;
         run ~secrets:Config.ssh_secrets ~network "rsync -avz /home/opam/html/ %s:%s/html"
           Config.ssh_host Config.storage_folder;
       ]

let v packages =
  let open Current.Syntax in
  let spec =
    let+ packages = packages in
    spec ~base:(Spec.make "ocaml/opam:ubuntu-ocaml-4.12") packages |> Spec.to_ocluster_spec
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:(Current.return [])
      ~pool:"linux-arm64" ~cache_hint:"docs-universe-link" cluster spec
  in
  ()
