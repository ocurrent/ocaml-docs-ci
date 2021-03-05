let opam_download_cache =
  Obuilder_spec.Cache.v "download-cache" ~target:"/home/opam/.opam/download-cache"

let network = [ "host" ]

let remote_uri commit =
  let repo = Current_git.Commit_id.repo commit in
  let commit = Current_git.Commit_id.hash commit in
  repo ^ "#" ^ commit

let add_repositories =
  List.map (fun (name, commit) ->
      Obuilder_spec.run ~network "opam repo add %s %s" name (remote_uri commit))

let install_tools tools =
  let tools_s = String.concat " " tools in
  [ Obuilder_spec.run ~network ~cache:[ opam_download_cache ] "opam depext -i %s" tools_s ]

module Op = struct
  type t = Name of string

  module Key = struct
    type t = { system : Platform.system; packages : Current_solver.resolution list }

    let digest t =
      let open Current_solver in
      let json =
        `List
          [
            `Assoc
              (List.map (fun a -> (a.name ^ a.version, Opamfile.to_yojson a.opamfile)) t.packages);
            `String (Fmt.str "%a" Platform.pp_system t.system);
          ]
      in
      Yojson.Safe.to_string json
  end

  module Value = Current_docker.Default.Image

  let id = "docker-tools-setup"

  let pp f t = Fmt.pf f "docker tools setup %a" Platform.pp_system t.Key.system

  let auto_cancel = true

  let spec ~system ~pkgs =
    let open Obuilder_spec in
    Platform.spec system
    |> Spec.add
         [
           workdir "/repo";
           copy [ "." ] ~dst:"/repo";
           run "opam repo remove default";
           run "opam repo add local /repo";
           run "opam depext -i %s" (String.concat " " pkgs);
         ]
    |> Spec.finish

  let setup_package (package : Current_solver.resolution) =
    let open Fpath in
    let dirname = v package.name / (package.name ^ "." ^ package.version) in
    let _ = Bos.OS.Dir.create dirname |> Result.get_ok in
    Bos.OS.File.write (dirname / "opam") (Opamfile.marshal package.opamfile) |> Result.get_ok

  let build (Name tool_name) job { Key.system; packages } =
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
    let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true (spec ~system ~pkgs) in
    Bos.OS.File.write (tmpdir / "Dockerfile") dockerfile |> Result.get_ok;
    (* use docker build *)
    let tool_name = Astring.String.map (function | ' ' -> '-' | c -> c) tool_name in
    let tag = Fmt.str "%s-%a" tool_name Platform.pp_system system in
    let cmd =
      Current_docker.Raw.Cmd.docker ~docker_context:None
        [ "build"; "-t"; tag; "--"; to_string tmpdir ]
    in
    let+ res = Current.Process.exec ~cancellable:true ~job cmd in
    Result.map (fun () -> Current_docker.Default.Image.of_hash tag) res
end

module SetupCache = Current_cache.Make (Op)

let tools_image ~system ?(name = "setup tools")
    (resolutions : Current_solver.resolution list Current.t) =
  let open Current.Syntax in
  Current.component "%s" name
  |> let> resolutions = resolutions in
     SetupCache.get (Name name) { Op.Key.packages = resolutions; system }
