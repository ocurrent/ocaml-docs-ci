let opam_download_cache =
  Obuilder_spec.Cache.v "opam-download-cache" ~target:"/home/opam/.opam/download-cache"

let network = [ "host" ]

let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit

let add_repositories = List.map (fun (name, commit) -> Obuilder_spec.run ~network "opam repo add %s %s" name (remote_uri commit))

let install_tools tools =
  let tools_s = String.concat " " tools in
  [
    Obuilder_spec.run ~network ~cache:[ opam_download_cache ] "opam depext -i %s" tools_s;
  ]

module Op = struct
  type t = No_context

  module Key = struct
    type t = Current_solver.resolution list

    let digest t =
      let open Current_solver in
      let json =
        `Assoc (List.map (fun a -> (a.name ^ a.version, Opamfile.to_yojson a.opamfile)) t)
      in
      Yojson.Safe.to_string json
  end

  module Value = Current_docker.Default.Image

  let id = "docker-tools-setup"

  let pp f _ = Fmt.string f "docker tools setup"

  let auto_cancel = true

  let spec ~pkgs =
    let open Obuilder_spec in
    stage ~from:"ocaml/opam:ubuntu-ocaml-4.11"
    @@ [
         user ~uid:1000 ~gid:1000;
         workdir "/repo";
         copy [ "." ] ~dst:"/repo";
         run "opam repo remove default";
         run "opam repo add local /repo";
         run "opam depext -i %s" (String.concat " " pkgs);
       ]

  let setup_package (package : Current_solver.resolution) =
    let open Fpath in
    let dirname = v package.name / (package.name ^ "." ^ package.version) in
    let _ = Bos.OS.Dir.create dirname |> Result.get_ok in
    Bos.OS.File.write (dirname / "opam") (Opamfile.marshal package.opamfile) |> Result.get_ok

  let build No_context job packages =
    let open Lwt.Syntax in
    let open Fpath in
    let* () = Current.Job.start ~level:Harmless job in
    Current.Process.with_tmpdir @@ fun tmpdir ->
    (* setup the context *)
    let () = Bos.OS.File.write (tmpdir / "repo") "opam-version: \"2.0\"" |> Result.get_ok in
    let _ = Bos.OS.Dir.create (tmpdir / "packages") |> Result.get_ok in
    Bos.OS.Dir.with_current (tmpdir / "packages") (fun () -> List.iter setup_package packages) ()
    |> Result.get_ok;
    let pkgs =
      List.map (fun (pkg : Current_solver.resolution) -> pkg.name ^ "." ^ pkg.version) packages
    in
    let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true (spec ~pkgs) in
    Bos.OS.File.write (tmpdir / "Dockerfile") dockerfile |> Result.get_ok;
    (* use docker build *)
    let iidfile = tmpdir / "iidfile" in
    let cmd =
      Current_docker.Raw.Cmd.docker ~docker_context:None
        [ "build"; "--iidfile"; to_string iidfile; "--"; to_string tmpdir ]
    in
    let+ res = Current.Process.exec ~cancellable:true ~job cmd in
    Result.bind res (fun () -> Bos.OS.File.read iidfile)
    |> Result.map (fun iid -> Current_docker.Default.Image.of_hash iid)
end

module SetupCache = Current_cache.Make (Op)

let tools_image ?(name = "setup tools") (resolutions : Current_solver.resolution list Current.t) =
  let open Current.Syntax in
  Current.component "%s" name
  |> let> resolutions = resolutions in
     SetupCache.get No_context resolutions
