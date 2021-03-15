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
           run ~network:Builder.network ~cache:Builder.cache "opam pin add odoc %s -ny" Config.odoc;
           run ~network:Builder.network ~cache:Builder.cache "opam depext -iy odoc";
           copy ~from:`Context [ "." ] ~dst:"/src/";
           run ~network:Builder.network "sudo apt-get install time";
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
