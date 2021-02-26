let pool = Current.Pool.create ~label:"mirage-pool" 4

open Current.Syntax
module Docker = Current_docker.Default

type t = Docker.Image.t

let v ~system ~repos =
  Current_solver.v ~system ~repos ~packages:[ "mirage" ]
  |> Setup.tools_image ~system ~name:"mirage tool"

let unikernel_find_cmd =
  "find . -maxdepth 1 -type f -not -name *install.opam -name *.opam -exec cat {} +"
  |> String.split_on_char ' '

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
  let+ opamfile =
    Docker.pread ~label:"read mirage configure output" image ~args:unikernel_find_cmd
  in
  OpamParser.string opamfile "monorepo.opam"

let build ?(cmd = "dune build") ~(platform : Matrix.platform) ~base ~project ~unikernel ~target () =
  let spec =
    let+ base = base in
    let open Obuilder_spec in
    base
    |> Spec.add (Setup.install_tools [ "dune"; "mirage"; "opam-monorepo"; "ocamlfind.1.8.1" ])
    |> Spec.add
         [
           copy [ "." ] ~dst:"/src/";
           workdir ("/src/" ^ unikernel);
           run "opam exec -- mirage configure -t %s" target;
           run ~cache:[ Setup.opam_download_cache ] ~network:Setup.network
             "opam exec -- make depends";
           run "opam exec -- %s" cmd;
         ]
  in
  let label = unikernel ^ "@" ^ target in
  let src = [ project ] |> Current.list_seq in
  let cache_hint = Fmt.str "mirage-ci-skeleton-%a" Matrix.pp_system platform.system in
  let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
  Current_ocluster.build_obuilder ~label ~cache_hint cluster ~pool:(Matrix.ocluster_pool platform)
    ~src (spec |> Config.to_ocluster_spec)
