module Git = Current_git

type t = { package : Package.t }

let package t = t.package

let network = Voodoo.network

let cache = Voodoo.cache

let not_base x =
  not
    (List.mem (OpamPackage.name_to_string x)
       [ "base-unix"; "base-bigarray"; "base-threads"; "ocaml-config"; "ocaml" ])

let folder t =
  let t = t.package in
  let universe = Package.universe t |> Package.Universe.hash in
  let opam = Package.opam t in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  Fpath.(v "prep" / "universes" / universe / name / version)

let make_base_folder package =
  Obuilder_spec.run "mkdir -p %s/" (folder { package } |> Fpath.to_string)

let universes_assoc packages =
  packages 
  |> List.map (fun pkg -> 
    let hash = pkg |> Package.universe |> Package.Universe.hash in 
    let name = pkg |> Package.opam |> OpamPackage.name_to_string in 
    name^":"^hash) 
  |> String.concat ","


let spec ~voodoo ~base (packages : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let packages_str =
    packages |> List.map Package.opam |> List.filter not_base |> List.map OpamPackage.to_string
    |> String.concat " "
  in
  Voodoo.spec ~base Prep voodoo 
  |> Spec.add
       ( [
           (* Install required packages *)
           copy [ "." ] ~dst:"/src";
           run "opam repo remove default && opam repo add opam /src";
           run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
         ]
       @ List.map make_base_folder packages (* empty preps should yield an empty folder *)
       @ [
           run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc packages);
           (* Perform the prep step for all packages *)
           run ~secrets:Config.ssh_secrets ~network:Voodoo.network "rsync -avz prep %s:%s/"
             Config.ssh_host Config.storage_folder;
         ] )

(** Assumption: packages are co-installable *)
let v ~voodoo (package : Package.t Current.t) =
  let open Current.Syntax in
  let opam_context =
    let+ package = package in
    [
      Current_git.Commit_id.v ~repo:"https://github.com/ocaml/opam-repository.git" ~gref:"master"
        ~hash:(Package.commit package);
    ]
  in
  let spec =
    let+ root = package and+ voodoo = voodoo in
    spec ~voodoo ~base:(Misc.get_base_image root) (Package.all_deps root) |> Spec.to_ocluster_spec
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    let* package = package in
    Current_ocluster.build_obuilder
      ~label:(Fmt.str "prep %a" Package.pp package)
      ~src:opam_context ~pool:Config.pool ~cache_hint:"docs-universe-build" cluster spec
  and+ root = package in
  List.map (fun package -> { package }) (Package.all_deps root)
