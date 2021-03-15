let network = Voodoo.network

let cache = Voodoo.cache

let not_base x =
  not
    (List.mem (OpamPackage.name_to_string x)
       [
         "base-unix";
         "base-bigarray";
         "base-threads";
         "ocaml-base-compiler";
         "ocaml-config";
         "ocaml";
       ])


let spec ~branch ~base (packages : OpamPackage.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let packages_str =
    packages |> List.filter not_base |> List.map OpamPackage.to_string |> String.concat " "
  in
  Voodoo.spec ~base ~prep:true ~link:false
  |> Spec.add (Worker_git.ops @
       [
         (* Install required packages *)
         copy ["."] ~dst:"/src"; 
         run "opam repo remove default && opam repo add opam /src";
         run ~network ~cache "opam depext -viy %s" packages_str;
         run "~/voodoo_prep";
         run "find prep -type d";
         run ~network "git clone %s -b base --single-branch" Config.v.remote_push;
         workdir "/home/opam/docs-ocaml-artifacts";
         run "git checkout -b %s" branch;
         run "mv ~/prep .";
         run "git add *";
         run "git commit -m 'Docs CI' --author 'Docs CI pipeline <ci@docs.ocaml.org>'";
         run ~network "git push -v -f origin %s" branch;
       ])

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let v ~commit ~base (root : OpamPackage.t Current.t) (packages : OpamPackage.t list Current.t) =
  let open Current.Syntax in
  let branch =
    let+ root = root in
    OpamPackage.to_string root
  in
  let opam_context = 
    let+ commit = commit in 
    [Current_git.Commit_id.v ~repo:"https://github.com/ocaml/opam-repository.git" ~gref:"master" ~hash:commit]
  in
  let spec =
    let+ packages = packages and+ branch = branch and+ base = base in
    spec ~branch ~base packages
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:opam_context
      ~pool:"linux-x86_64" ~cache_hint:"docs-universe-build" cluster
      (spec |> Config.to_ocluster_spec)
  and+ branch = branch in
  let open Bos in
  let res =
    OS.Cmd.run_out Cmd.(v "git" % "ls-remote" % Config.v.remote_pull % branch)
    |> OS.Cmd.to_string |> Result.get_ok
  in
  let commit = String.split_on_char '\t' res |> List.hd in
  Current_git.Commit_id.v ~repo:Config.v.remote_pull ~gref:branch ~hash:commit
