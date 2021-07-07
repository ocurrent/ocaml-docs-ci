(* Opam info *)

let id = "metadata-v2"

let sync_pool = Current.Pool.create ~label:"ssh" 1

let state_dir = Current.state_dir id

module Metadata = struct
  type t = { ssh : Config.Ssh.t }

  (* Key is 'opam_metadata' always, value is the commit id of the opam repository *)

  let id = "update-metadata"

  let auto_cancel = true

  module Key = struct
    type t = Epoch.t

    let digest v = Fmt.str "metadata4-%s" (Epoch.digest v)
  end

  module Value = struct
    type t = Current_git.Commit.t

    let digest = Fmt.to_to_string Current_git.Commit.pp
  end

  module Outcome = Current.String

  let pp fmt (_k, v) = Format.fprintf fmt "metadata-%a" Current_git.Commit.pp v

  let rec take n lst =
    match (n, lst) with 0, _ -> [] | _, [] -> [] | n, a :: q -> a :: take (n - 1) q

  let take = function Some n -> take n | None -> Fun.id

  let get_digest path =
    let content = Bos.OS.File.read path |> Result.get_ok in
    Digestif.SHA256.(digest_string content |> to_hex)

  let initialize_state ~generation ~job ~ssh () =
    let port = Config.Ssh.port ssh in
    let user = Config.Ssh.user ssh in
    let privkeyfile = Config.Ssh.priv_key_file ssh in
    let host = Config.Ssh.host ssh in
    let root_folder = Config.Ssh.storage_folder ssh in
    Current.Process.exec ~cancellable:false ~job
      ( "",
        Bos.Cmd.(
          v "rsync" % "-avzR" % "--delete" % "-e"
          % Fmt.str "ssh -p %d -i %a" port Fpath.pp privkeyfile
          % Fmt.str "--rsync-path=mkdir -p %s/%a && rsync" root_folder Fpath.pp
              (Storage.Base.folder (HtmlTailwind generation))
          % Fmt.str "%s@%s:%s/./%a" user host root_folder Fpath.pp
              (Storage.Base.folder (HtmlTailwind generation))
          % Fmt.str "%a/./" Fpath.pp state_dir)
        |> Bos.Cmd.to_list |> Array.of_list )

  let get_versions path =
    let open Rresult in
    Bos.OS.Dir.contents path
    >>| (fun versions ->
          versions
          |> List.rev_map (fun path ->
                 (path |> Fpath.basename |> OpamPackage.of_string, get_digest Fpath.(path / "opam"))))
    |> Result.get_ok
    |> List.sort (fun (a, _) (b, _) -> OpamPackage.compare a b)
    |> List.rev

  let write_state ~generation ~job ~repo =
    let open Rresult in
    let open Lwt.Syntax in
    let open OpamPackage in
    Current_git.with_checkout ~job repo @@ fun dir ->
    let packages = OpamRepository.packages (OpamFilename.Dir.of_string (Fpath.to_string dir)) in
    let versions =
      Set.fold
        (fun pkg map ->
          let name = name pkg in
          let version = version pkg in
          Name.Map.update name
            (fun versions -> Version.Set.add version versions)
            Version.Set.empty map)
        packages Name.Map.empty
    in
    let* () =
      Lwt_stream.iter_s
        (fun (name, versions) ->
          let dir =
            Fpath.(
              state_dir
              // Storage.Base.folder (HtmlTailwind generation)
              / "packages" / OpamPackage.Name.to_string name)
          in
          let file = Fpath.(dir / "package.json") in
          Sys.command (Format.asprintf "mkdir -p %a" Fpath.pp dir) |> ignore;
          let* file = Lwt_io.open_file ~mode:Output (Fpath.to_string file) in
          let json = OpamPackage.Version.Set.to_json versions in
          let* () = Lwt_io.write file (OpamJson.to_string json) in
          Lwt_io.close file)
        (Lwt_stream.of_seq (OpamPackage.Name.Map.to_seq versions))
    in
    Lwt.return (Ok ())

  let send_state ~generation ~job ~ssh () =
    let port = Config.Ssh.port ssh in
    let user = Config.Ssh.user ssh in
    let privkeyfile = Config.Ssh.priv_key_file ssh in
    let host = Config.Ssh.host ssh in
    let root_folder = Config.Ssh.storage_folder ssh in
    Current.Process.exec ~cancellable:false ~job
      ( "",
        Bos.Cmd.(
          v "rsync" % "-avzR" % "-e"
          % Fmt.str "ssh -p %d -i %a" port Fpath.pp privkeyfile
          % (Fpath.to_string state_dir ^ "/./")
          % Fmt.str "%s@%s:%s" user host root_folder)
        |> Bos.Cmd.to_list |> Array.of_list )

  let hash_state ~job () =
    let ( let** ) = Lwt_result.bind in
    let** () =
      Current.Process.exec ~cancellable:false ~job
        ( "",
          [|
            "bash";
            "-c";
            Fmt.str "find %a -type f -name '*.json' -maxdepth 5 -exec sha256sum {} \\;" Fpath.pp
              state_dir;
          |] )
    in
    Current.Process.check_output ~cancellable:false ~job
      ( "",
        [|
          "bash";
          "-c";
          Fmt.str
            "find %a -type f -name '*.json' -maxdepth 5 -exec sha256sum {} \\; | sort | sha256sum"
            Fpath.pp state_dir;
        |] )

  let publish { ssh } job generation v =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let switch = Current.Switch.create ~label:"sync" () in
    Lwt.finalize
      (fun () ->
        let* () = Current.Job.start_with ~pool:sync_pool ~level:Mostly_harmless job in
        let** () = initialize_state ~generation ~job ~ssh () in
        let** () = write_state ~generation ~job ~repo:v in
        let** () = send_state ~generation ~job ~ssh () in
        hash_state ~job ())
      (fun () -> Current.Switch.turn_off switch)
end

module MetadataCache = Current_cache.Output (Metadata)

let v ~ssh ~generation ~(repo : Current_git.Commit.t Current.t) =
  let open Current.Syntax in
  Current.component "set-status"
  |> let> repo = repo and> generation = generation in
     MetadataCache.set { ssh } generation repo
