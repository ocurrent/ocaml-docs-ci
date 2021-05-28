(* Opam info *)

let id = "metadata"

let sync_pool = Current.Pool.create ~label:"ssh" 1

let state_dir = Current.state_dir id

module Metadata = struct
  type t = { ssh : Config.Ssh.t }

  (* Key is 'opam_metadata' always, value is the commit id of the opam repository *)

  let id = "update-metadata"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest v = Format.asprintf "metadata3-%s" v
  end

  module Value = struct
    type t = Current_git.Commit.t

    let digest = Fmt.to_to_string Current_git.Commit.pp
  end

  module Outcome = Current.Unit

  let pp fmt (_k, v) = Format.fprintf fmt "metadata-%a" Current_git.Commit.pp v

  let rec take n lst =
    match (n, lst) with 0, _ -> [] | _, [] -> [] | n, a :: q -> a :: take (n - 1) q

  let take = function Some n -> take n | None -> Fun.id

  let get_digest path =
    let content = Bos.OS.File.read path |> Result.get_ok in
    Digestif.SHA256.(digest_string content |> to_hex)

  let initialize_state ~job ~ssh () =
    if Bos.OS.Path.exists Fpath.(state_dir / ".git") |> Result.get_ok then Lwt.return_ok ()
    else
      Current.Process.exec ~cancellable:false ~job
        ( "",
          Git_store.Local.clone ~branch:"status" ~directory:state_dir ssh
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

  let write_state ~job ~repo =
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
          let dir = Fpath.(state_dir / "html" / "packages" / OpamPackage.Name.to_string name) in
          let file = Fpath.(dir / "package.json") in
          Sys.command (Format.asprintf "mkdir -p %a" Fpath.pp dir) |> ignore;
          let* file = Lwt_io.open_file ~mode:Output (Fpath.to_string file) in
          let json = OpamPackage.Version.Set.to_json versions in
          let* () = Lwt_io.write file (OpamJson.to_string json) in
          Lwt_io.close file)
        (Lwt_stream.of_seq (OpamPackage.Name.Map.to_seq versions))
    in
    Lwt.return (Ok ())

  let publish { ssh } job _ v =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let switch = Current.Switch.create ~label:"sync" () in
    let* () = Current.Job.start_with ~pool:sync_pool ~level:Mostly_harmless job in
    Lwt.finalize
      (fun () ->
        let** () = initialize_state ~job ~ssh () in
        let* _ = write_state ~job ~repo:v in
        let** () =
          Current.Process.exec ~cancellable:true ~cwd:state_dir ~job
            ( "",
              [|
                "bash";
                "-c";
                Fmt.str
                  "git add --all && (git diff HEAD --exit-code --quiet || git commit -m 'update \
                   status')";
              |] )
        in
        let* _ =
          Current.Process.exec ~cancellable:true ~job
            ("", Git_store.Local.push ~directory:state_dir ssh |> Bos.Cmd.to_list |> Array.of_list)
        in
        Git_store.Local.merge_to_live ~job ~ssh ~branch:"status" ~msg:"Update opam metadata")
      (fun () -> Current.Switch.turn_off switch)
end

module MetadataCache = Current_cache.Output (Metadata)

let v ~ssh ~(repo : Current_git.Commit.t Current.t) : unit Current.t =
  let open Current.Syntax in
  Current.component "set-status"
  |> let> repo = repo in
     MetadataCache.set { ssh } "metadata" repo
