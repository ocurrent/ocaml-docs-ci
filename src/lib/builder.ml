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

let ssh_config =
  {|Host ci.mirage.io
    IdentityFile ~/.ssh/id_rsa
    Port 10022
    User git
    StrictHostKeyChecking=no
|}

let spec ~branch ~repo ~base (packages : OpamPackage.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let packages_str =
    packages |> List.filter not_base |> List.map OpamPackage.to_string |> String.concat " "
  in
  base
  |> Spec.add
       [
         run ~network "sudo apt-get update && sudo apt-get install -yy m4";
         (* Install tools *)
         run ~network ~cache "opam repo remove default && opam repo add opam %s " repo;
         (* Enable binary cache *)
         run ~network
           "curl https://raw.githubusercontent.com/ocaml/opam/2.0.8/shell/opam-bin-cache.sh -O && \
            chmod +x opam-bin-cache.sh && mv opam-bin-cache.sh /home/opam/.opam/opam-init/hooks/";
         run "cat /home/opam/.opam/config";
         (*  run "for i in {1..3}; do sed -i '$d' /home/opam/.opam/config; done; ";*)
         run "echo '%s' >> /home/opam/.opam/config" build_cache_config;
         run "cat /home/opam/.opam/config";
         (* Update opam *)
         env "OPAMPRECISETRACKING" "1";
         (* NOTE: See https://github.com/ocaml/opam/issues/3997 *)
         env "OPAMDEPEXTYES" "1";
         run ~network ~cache "opam pin add -y git://github.com/jonludlam/voodoo-prep";
         run "cp $(opam config var bin)/voodoo_prep /home/opam";
         run "opam remove -ay voodoo-prep";
       ]
  |> Spec.add
       [
         run ~network ~cache "opam depext -viy %s" packages_str;
         run "~/voodoo_prep";
         run "find prep -type d";
         run "echo '%s' >> .ssh/id_rsa && chmod 600 .ssh/id_rsa" Key.priv;
         run "echo '%s' >> .ssh/id_rsa.pub" Key.pub;
         run "echo '%s' >> .ssh/config" ssh_config;
         run ~network "git clone %s -b base --single-branch" Config.v.remote_push;
         workdir "/home/opam/docs-ocaml-artifacts";
         run "git checkout -b %s" branch;
         run "mv ~/prep .";
         run "git add *";
         run "git commit -m 'Docs CI' --author 'Docs CI pipeline <ci@docs.ocaml.org>'";
         run ~network "git push -v -f origin %s" branch;
       ]

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let v ~opam ~base (root : OpamPackage.t Current.t) (packages : OpamPackage.t list Current.t) =
  let open Current.Syntax in
  let repo = remote_uri (Current_git.Commit.id opam) in
  let branch =
    let+ root = root in
    OpamPackage.to_string root
  in
  let spec =
    let+ packages = packages and+ branch = branch and+ base = base in
    spec ~branch ~repo ~base packages
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:(Current.return [])
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
