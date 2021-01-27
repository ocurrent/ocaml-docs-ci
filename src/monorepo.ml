

let pool = Current.Pool.create ~label:"git clone" 8

open Lwt.Infix
open Current.Syntax

let or_raise = function
  | Ok () -> ()
  | Error (`Msg m) -> raise (Failure m)

module Assembler = struct 
  type t = (string * string) list (* opam repositories // TODO *)

  let id = "mirage-ci-monorepo-assembler"

  module Key = struct 

    type t = (string * Current_git.Commit.t) list

    let digest lst = List.map (fun (_,x) -> Current_git.Commit.hash x) lst |> String.concat ";" 

  end
    
  module Value = Current_docker.Default.Image

  let opam_monorepo_spec ~repos = 
    let open Obuilder_spec in 
    stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
    @@
    Setup.install_tools ~repos ~tools:["dune"]
    @ [
      workdir "/src/";
      run "sudo chown opam /src/";
      user ~uid:1000 ~gid:1000;
      copy ["."] ~dst:"/src/";
    ]

  let build repos job commits = 
    Current.Job.start ~level:Harmless job >>= fun () ->
    Current.Process.with_tmpdir (fun monorepo_path -> 
      List.map (fun (name, commit) -> 
        Current_git.with_checkout ~pool ~job commit 
          (fun repo_path -> 
            let cmd = Bos.Cmd.(v "cp" % "-r" % (Fpath.to_string repo_path) % (Fpath.(to_string (monorepo_path / name)))) in
            Bos.OS.Cmd.run cmd |> or_raise;
            Lwt.return_ok ()
      )) commits
      |> Lwt.all >>= fun _ -> 
        Current.Job.log job "Cloned every git repository in %a" Fpath.pp monorepo_path;
        let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false (opam_monorepo_spec ~repos) in
        Bos.OS.File.write Fpath.(monorepo_path / "Dockerfile") dockerfile |> or_raise;
        let iidfile = Fpath.(monorepo_path / "image.id") in
        let cmd =
          Current_docker.Raw.Cmd.docker ~docker_context:None
            [ "build"; "--iidfile"; Fpath.to_string iidfile;"--"; Fpath.to_string monorepo_path ]
        in
        Current.Process.exec ~cancellable:true ~job cmd >|= fun res ->
        Result.bind res (fun () -> Bos.OS.File.read iidfile)
        |> Result.map (fun id -> Current_docker.Default.Image.of_hash id)
    ) 

  let pp f _ = Fmt.string f "Monorepo assembler"

  let auto_cancel = true

end

module Cache = Current_cache.Make(Assembler)



let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

let monorepo_master ~repos ~projects () =
  Current.component "Monorepo assembler" |> 
  let> repos = Current.list_seq repos
  and> projects = Current.list_seq projects in 
  let repos = List.map (fun (a,b) -> a, remote_uri b) repos in
  Cache.get repos projects

