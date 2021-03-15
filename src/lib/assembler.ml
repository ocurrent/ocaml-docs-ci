let spec ~base =
  let open Obuilder_spec in
  Voodoo.spec ~base ~prep:false ~link:true
  |> Spec.add
       ( Worker_git.ops
       @ [
           run ~network:Builder.network ~cache:Builder.cache "opam pin add odoc %s -ny" Config.odoc;
           run ~network:Builder.network ~cache:Builder.cache "opam depext -iy odoc";
           copy ~from:`Context [ "." ] ~dst:"/src/";
           run ~network:Builder.network "sudo apt-get install time";
           workdir "/src";
           run "opam exec -- ~/voodoo-link compile";
           run "make -t -f Makefile.gen compile";
           run "opam exec -- make -f Makefile.mlds compile";
           run "opam exec -- make -f Makefile.mlds link";
           run "opam exec -- make -f Makefile.link link";
           run
             "for i in `find . -name \"*.odocl\"`; do `opam var bin`/odoc html-generate $i -o html; \
              done";
           run "opam exec -- odoc support-files -o html";
           workdir "/src/html";
           (* artifacts upload *)
           run "git init";
           run "git remote add origin %s" Config.v.remote_push;
           run "git checkout -b main";
           run "git add *";
           run "git commit -m 'Docs CI' --author 'Docs CI pipeline <ci@docs.ocaml.org>'";
           run ~network:Builder.network "git push -v -f origin main";
         ] )

let v ~base prep_branches link_branches =
  let open Current.Syntax in
  let branches =
    let+ pb = prep_branches and+ lb = link_branches in
    pb @ lb
  in
  let spec =
    let+ base = base in
    spec ~base
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v conn in
  Current_ocluster.build_obuilder ~label:"cluster build" ~src:branches ~pool:"linux-arm64"
    ~cache_hint:"docs-universe-build" cluster (spec |> Config.to_ocluster_spec)
