type t = Package.t

let network = Voodoo.network

let cache = Voodoo.cache

let spec ~branch ~base (blessed : OpamPackage.t list) =
  let whitelist = blessed |> List.map OpamPackage.name_to_string |> String.concat "," in
  let open Obuilder_spec in
  Voodoo.spec ~base ~prep:false ~link:true
  |> Spec.add
       ( Worker_git.ops
       @ [
           (* Install required packages *)
           run ~network:Voodoo.network ~cache:Voodoo.cache "opam pin add odoc %s -ny" Config.odoc;
           run ~network:Voodoo.network ~cache:Voodoo.cache "opam depext -iy odoc";
           copy ~from:`Context [ "." ] ~dst:"/src/";
           run ~network:Voodoo.network "sudo apt-get install time";
           (* a prepped folder should be here *)
           workdir "/src";
           run "opam exec -- ~/voodoo-link compile -w %s" whitelist;
           run "find compile -type d";
           run "opam exec -- make -f Makefile.mlds compile";
           run "opam exec -- make -f Makefile.gen compile";
           run ~network "git clone %s -b base --single-branch" Config.v.remote_push;
           workdir "/src/docs-ocaml-artifacts";
           run "git checkout -b %s" branch;
           run "mv ../compile/ .";
           run "rm compile/*.mld compile/*.odoc compile/packages/*.mld compile/packages/*.odoc";
           run "git add *";
           run "git commit -m 'Docs CI' --author 'Docs CI pipeline <ci@docs.ocaml.org>'";
           run ~network "git push -v -f origin %s" branch;
         ] )

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

(*
let v ~base (prep : Current_git.Commit_id.t Current.t) (blessed : OpamPackage.t list Current.t) =
  let open Current.Syntax in
  let branch =
    let+ prep = prep in
    "link-" ^ (Current_git.Commit_id.gref prep)
  in
  let spec =
    let+ blessed = blessed and+ branch = branch and+ base = base in
    spec ~branch ~base blessed
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build"
      ~src:(Current.map (fun x -> [ x ]) prep)
      ~pool:"linux-arm64" ~cache_hint:"docs-universe-build" cluster
      (spec |> Config.to_ocluster_spec)
  and+ branch = branch in
  let open Bos in
  let res =
    OS.Cmd.run_out Cmd.(v "git" % "ls-remote" % Config.v.remote_pull % branch)
    |> OS.Cmd.to_string |> Result.get_ok
  in
  let commit = String.split_on_char '\t' res |> List.hd in
  Current_git.Commit_id.v ~repo:Config.v.remote_pull ~gref:branch ~hash:commit
*)

let ssh_config =
  {|Host ci.mirage.io
    IdentityFile ~/.ssh/id_rsa
    User docs
    StrictHostKeyChecking=no
|}

let folder ~blessed t =
  let universe = Package.universe t |> Package.Universe.hash in
  let opam = Package.opam t in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if Package.Blessed.is_blessed blessed t then Fmt.str "/compile/packages/%s/%s/" name version
  else Fmt.str "/compile/universes/%s/%s/%s" universe name version

let cache = [ Obuilder_spec.Cache.v ~target:"/home/.opam/docs/" "ci-docs" ]

let privkey = Obuilder_spec.Secret.v ~target:"/home/opam/.ssh/id_rsa" "privkey"

let pubkey = Obuilder_spec.Secret.v ~target:"/home/opam/.ssh/id_rsa.pub" "pubkey"

let import_dep ~blessed dep =
  let folder = folder ~blessed dep in
  Obuilder_spec.run ~secrets:[ privkey; pubkey ] ~cache ~network
    "rsync -avzR ci.mirage.io:/home/docs/docs/./%s /home/opam/docs/" folder 

let spec ~base ~blessed ~deps target =
  let open Obuilder_spec in
  let prep_folder = Prep.folder target in
  let compile_folder = folder ~blessed (Prep.package target) in
  base
  |> Spec.add
       ( [ run "echo '%s' >> ~/.ssh/config" ssh_config ]
       @ List.map (import_dep ~blessed) deps
       @ [
           run ~secrets:[ privkey; pubkey ] ~cache ~network
             "rsync -avzR ci.mirage.io:/home/docs/docs/./%s /home/opam/docs/" prep_folder;
           run "voodoo-link compile";
           run ~secrets:[ privkey; pubkey ] ~network "rsync -avzR /home/opam/docs/./%s ci.mirage.io:/home/docs/docs/%s"
             compile_folder compile_folder;
         ] )

let v ~blessed ~deps target =
  let open Current.Syntax in
  let spec =
    let+ deps = deps and+ blessed = blessed and+ target = target in
    spec ~base:(Misc.get_base_image (Prep.package target)) ~blessed ~deps target
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster =
    Current_ocluster.v ~secrets:[ ("privkey", Key.ssh_priv); ("pubkey", Key.ssh_pub) ] conn
  in
  let+ () =
    Current_ocluster.build_obuilder ~label:"cluster build" ~src:(Current.return [])
      ~pool:"linux-arm64" ~cache_hint:"docs-universe-build" cluster (spec |> Config.to_ocluster_spec)
  and+ target = target in
  Prep.package target
