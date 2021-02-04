let pool = Current.Pool.create ~label:"mirage-pool" 4

open Current.Syntax
module Docker = Current_docker.Default

type t = Docker.Image.t

let v ~repos =
  Current_solver.v ~repos ~packages:["mirage"]
  |> Setup.tools_image ~name:"mirage tool"

let unikernel_find_cmd = "find . -maxdepth 1 -type f -not -name *install.opam -name *.opam -exec cat {} +" |> String.split_on_char ' '

let configure ~project ~unikernel ~target t =
  let dockerfile =
    let+ t = t in
    let open Dockerfile in
    from (Docker.Image.hash t)
    @@ user "opam"
    @@ copy ~chown:"opam" ~src:[ "." ] ~dst:"/src" ()
    @@ workdir "/src/%s" unikernel
    @@ run "opam exec -- mirage configure -t %s" target
    |> fun dockerfile -> `Contents dockerfile
  in
  let image =
    Docker.build ~dockerfile
      ~label:("mirage configure " ^ unikernel ^ " @" ^ target)
      ~pool ~pull:false (`Git project)
  in
  let+ opamfile = Docker.pread ~label:"read mirage configure output" image ~args:unikernel_find_cmd in
  OpamParser.string opamfile "monorepo.opam"

let build ~base ~project ~unikernel ~target =
  let spec =
    let+ base = base in
    let open Obuilder_spec in
    base
    |> Spec.add (Setup.install_tools [ "dune"; "mirage"; "opam-monorepo"; "ocamlfind.1.8.1" ])
    |> Spec.add
         [
           user ~uid:1000 ~gid:1000;
           copy [ "." ] ~dst:"/src/";
           workdir ("/src/" ^ unikernel);
           run "opam exec -- mirage configure -t %s" target;
           run ~network:Setup.network "opam exec -- make depends";
           run "opam exec -- dune build";
         ]
  in
  let src =
    let+ project = project in
    [ Current_git.Commit.id project ]
  in
  let cache_hint = "mirage-ci-skeleton" in
  let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
  [
    Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-arm64" ~src
      (spec |> Config.to_ocluster_spec);
    Current_ocluster.build_obuilder ~cache_hint cluster ~pool:"linux-x86_64" ~src
      (spec |> Config.to_ocluster_spec);
  ]
  |> Current.all
