module Git = Current_git

type t = { package : Package.t }

let package t = t.package

let network = Voodoo.network

let cache = Voodoo.cache

let not_base x =
  not
    (List.mem (OpamPackage.name_to_string x)
       [
         "base-unix";
         "base-bigarray";
         "base-threads";
         "ocaml-config";
         "ocaml";
       ])

let folder t =
  let t = t.package in
  let universe = Package.universe t |> Package.Universe.hash in
  let opam = Package.opam t in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  Fmt.str "/prep/universes/%s/%s/%s/" universe name version
      
        
let prep_rule package =
  (* the rule to extract a package installation *)
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let open Obuilder_spec in (* todo: use Package.prep_folder *)
  Obuilder_spec.run
    {|cat $(opam var prefix)/.opam-switch/install/%s.changes \
    | grep -oP '"\K.*(?=" {"F:)' \
    | grep '^doc/\|\.cmi$\|\.cmt$\|\.cmti$\|META$\|dune-package$' \
    | xargs -I '{}' install -D $(opam var prefix)'/{}' '/%s/{}'|}
    name (folder {package})

let make_base_folder package =
  let open Obuilder_spec in
  Obuilder_spec.run "mkdir -p /%s/" (folder {package})

let privkey = Obuilder_spec.Secret.v ~target:"/home/opam/.ssh/id_rsa" "privkey"

let pubkey = Obuilder_spec.Secret.v ~target:"/home/opam/.ssh/id_rsa.pub" "pubkey"

let ssh_config =
  {|Host ci.mirage.io
    IdentityFile ~/.ssh/id_rsa
    User docs
    StrictHostKeyChecking=no
|}

let spec ~base (packages : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let packages_str =
    packages |> List.map Package.opam |> List.filter not_base |> List.map OpamPackage.to_string
    |> String.concat " "
  in
  base
  |> Spec.add
       ( [
           (* Install required packages *)
           copy [ "." ] ~dst:"/src";
           run "opam repo remove default && opam repo add opam /src";
           run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
           run "sudo mkdir /prep && sudo chown opam:opam /prep";
         ]
      @ List.map make_base_folder packages (* empty preps should yield an empty folder *)
       @ List.map prep_rule packages (* Perform the prep step for all packages *)
       @ [
           run "echo '%s' >> /home/opam/.ssh/config" ssh_config;
           run ~secrets:[ privkey; pubkey ] ~network:Voodoo.network
             "rsync -avz /prep ci.mirage.io:~/docs/";
         ] )

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

(** Assumption: packages are co-installable *)
let v (package : Package.t Current.t) =
  let open Current.Syntax in
  let opam_context =
    let+ package = package in
    [
      Current_git.Commit_id.v ~repo:"https://github.com/ocaml/opam-repository.git" ~gref:"master"
        ~hash:(Package.commit package);
    ]
  in
  let spec =
    let+ root = package in
    spec ~base:(Misc.get_base_image root) (Package.all_deps root)
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster =
    Current_ocluster.v ~secrets:[ ("privkey", Key.ssh_priv); ("pubkey", Key.ssh_pub) ] conn
  in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:opam_context ~pool:"linux-arm64"
      ~cache_hint:"docs-universe-build" cluster (spec |> Config.to_ocluster_spec)
  and+ root = package in
  List.map (fun package -> { package }) (Package.all_deps root)
