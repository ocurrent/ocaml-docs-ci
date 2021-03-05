let pool = Current.Pool.create ~label:"mirage-pool" 4

open Current.Syntax
module Docker = Current_docker.Default
module Git = Current_git

type t = Docker.Image.t

let v ~system ~repos =
  Current_solver.v ~system ~repos ~packages:[ "mirage" ]
  |> Setup.tools_image ~system ~name:"mirage tool"

module ConfigureOp = struct
  type t = No_context

  let id = "mirage-tool-configure"

  let pp f _ = Fmt.pf f "mirage configure"

  module Key = struct
    type t = { tool : Docker.Image.t; project : Git.Commit.t; unikernel : string; target : string }

    let digest { tool; project; unikernel; target } =
      Fmt.str "%s-%s-%s-%s" (Docker.Image.digest tool) (Git.Commit.hash project) unikernel target
  end

  module Value = Opamfile

  let auto_cancel = true

  let cmd ~unikernel ~target =
    Fmt.str
      "cd /src/%s && mirage configure -t %s && find /src/%s -maxdepth 1 -type f -not -name \
       \"*install.opam\" -name \"*.opam\" -exec cat {} +"
      unikernel target unikernel

  let build No_context job { Key.tool; project; unikernel; target } =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~pool ~job project @@ fun dir ->
    let cmd =
      Current_docker.Raw.Cmd.docker ~docker_context:None
        [
          "run"; "--rm";
          "-v"; Fmt.str "%a:/src" Fpath.pp dir;
          "-u"; "opam";
          Docker.Image.hash tool;
          "bash"; "-c";
          cmd ~unikernel ~target;
        ]
    in
    let+ result = Current.Process.check_output ~cancellable:true ~job cmd in
    Result.map (fun opamfile -> OpamParser.string opamfile "monorepo.opam") result
end

module ConfigureCache = Current_cache.Make (ConfigureOp)

let configure ~project ~unikernel ~target t =
  Current.component "mirage configure"
  |> let> project = project and> t = t in
     ConfigureCache.get No_context { ConfigureOp.Key.tool = t; project; unikernel; target }

let build ?(cmd = "dune build") ~(platform : Platform.t) ~base ~project ~unikernel ~target () =
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
  let cache_hint = Fmt.str "mirage-ci-skeleton-%a" Platform.pp_system platform.system in
  let cluster = Current_ocluster.v (Current_ocluster.Connection.create Config.cap) in
  Current_ocluster.build_obuilder ~label ~cache_hint cluster ~pool:(Platform.ocluster_pool platform)
    ~src (spec |> Config.to_ocluster_spec)
