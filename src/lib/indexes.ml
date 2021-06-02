let id = "indexes"

let sync_pool = Current.Pool.create ~label:"ssh" 1

let state_dir = Current.state_dir id

module Index = struct
  type t = {
    ssh : Config.Ssh.t;
    state : Web.Status.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t ref;
  }

  let id = "update-status-metadata"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest package_name = Format.asprintf "status2-%s" package_name
  end

  module Value = struct
    type t = Web.Status.t OpamPackage.Version.Map.t OpamPackage.Name.Map.t

    let digest v =
      let digest = ref "" in
      OpamPackage.Name.Map.iter
        (fun name versions ->
          OpamPackage.Version.Map.iter
            (fun version status ->
              digest :=
                Digest.string
                  (Fmt.str "%s-%s-%s-%a" !digest (OpamPackage.Name.to_string name)
                     (OpamPackage.Version.to_string version)
                     Web.Status.pp status))
            versions)
        v;
      !digest
  end

  let pp f (package_name, _) = Fmt.pf f "Status update package %s" package_name

  module Outcome = Current.Unit

  type versions = { version : string; link : string; status : string } [@@deriving yojson]

  type v_list = versions list [@@deriving yojson]

  let mkdir_p d =
    let segs = Fpath.segs (Fpath.normalize d) |> List.filter (fun s -> String.length s > 0) in
    let init, segs = match segs with "" :: xs -> (Fpath.v "/", xs) | _ -> (Fpath.v ".", segs) in
    let _ =
      List.fold_left
        (fun path seg ->
          let d = Fpath.(path // v seg) in
          try
            Log.err (fun f -> f "mkdir %a" Fpath.pp d);
            Unix.mkdir (Fpath.to_string d) 0o755;
            d
          with
          | Unix.Unix_error (Unix.EEXIST, _, _) -> d
          | exn -> raise exn)
        init segs
    in
    ()

  let sync_pool = Current.Pool.create ~label:"ssh" 1

  let check_has_changed ~state name versions =
    let has_changed =
      match OpamPackage.Name.Map.find_opt name !state with
      | Some versions' -> versions <> versions'
      | None -> true
    in
    if has_changed then state := OpamPackage.Name.Map.add name versions !state;
    has_changed

  let write_state ~state ~job name versions =
    let open Lwt.Syntax in
    if check_has_changed ~state name versions then (
      let package_name = OpamPackage.Name.to_string name in
      Current.Job.log job "Updating state for %s" package_name;
      let dir = Fpath.(state_dir / package_name) in
      Sys.command (Format.asprintf "mkdir -p %a" Fpath.pp dir) |> ignore;
      let file = Fpath.(dir / "state.json") in
      let ts =
        OpamPackage.Version.Map.mapi
          (fun version status ->
            let version = OpamPackage.Version.to_string version in
            {
              version;
              link = Format.asprintf "/tailwind/packages/%s/%s/index.html" package_name version;
              status = Fmt.to_to_string Web.Status.pp status;
            })
          versions
        |> OpamPackage.Version.Map.values
      in
      let j = v_list_to_yojson ts in
      let* file = Lwt_io.open_file ~mode:Output (Fpath.to_string file) in
      let* () = Lwt_io.write file (Yojson.Safe.to_string j) in
      Lwt_io.close file)
    else Lwt.return ()

  let initialize_state ~job ~ssh () =
    if Bos.OS.Path.exists Fpath.(state_dir / ".git") |> Result.get_ok then Lwt.return_ok ()
    else
      Current.Process.exec ~cancellable:false ~job
        ( "",
          Git_store.Local.clone ~branch:"status" ~directory:state_dir ssh
          |> Bos.Cmd.to_list |> Array.of_list )

  let publish { ssh; state } job _ v =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let switch = Current.Switch.create ~label:"sync" () in
    Lwt.finalize
      (fun () ->
        let* () = Current.Job.start_with ~pool:sync_pool ~level:Mostly_harmless job in
        let** () = initialize_state ~job ~ssh () in
        let* () =
          OpamPackage.Name.Map.bindings v
          |> Lwt_list.iter_s (fun (name, versions) -> write_state ~job ~state name versions)
        in
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
        Current.Process.exec ~cancellable:true ~job
          ("", Git_store.Local.push ~directory:state_dir ssh |> Bos.Cmd.to_list |> Array.of_list))
      (fun () -> Current.Switch.turn_off switch)
end

module StatCache = Current_cache.Output (Index)

let v ~ssh ~statuses : unit Current.t =
  let state = ref OpamPackage.Name.Map.empty in
  let open Current.Syntax in
  Current.component "set-status"
  |> let> statuses = statuses in
     StatCache.set { state; ssh } "" statuses
