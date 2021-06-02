let id = "remote-cache"

let state_dir = Current.state_dir id

let sync_pool = Current.Pool.create ~label:"ssh" 1

let sync ~job t =
  let open Lwt.Syntax in
  let remote_folder =
    Fmt.str "%s@@%s:%s/" (Config.Ssh.user t) (Config.Ssh.host t) (Config.Ssh.storage_folder t)
  in
  let switch = Current.Switch.create ~label:"ssh" () in
  Lwt.finalize
    (fun () ->
      Current.Job.log job "Synchronizing remote cache.";
      let* () = Current.Job.use_pool ~switch job sync_pool in
      let+ _ =
        Current.Process.exec ~cancellable:true ~job
          ( "",
            [|
              "rsync";
              "-avzR";
              "--delete";
              "-e";
              Fmt.str "ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t) Fpath.pp
                (Config.Ssh.priv_key_file t);
              remote_folder ^ "/cache/./";
              Fpath.to_string state_dir;
            |] )
      in
      ())
    (fun () -> Current.Switch.turn_off switch)

type t = Config.Ssh.t

type cache_key = string

type digest = string

type build_result = Ok of digest | Failed

type cache_entry = (digest * build_result) option

let digest = function
  | None -> "none"
  | Some (k, Failed) -> "failed-" ^ k
  | Some (k, Ok digest) -> "ok-" ^ k ^ "-" ^ digest

let pp f = function
  | None -> Fmt.pf f "none"
  | Some (_, Failed) -> Fmt.pf f "failed"
  | Some (_, Ok digest) -> Fmt.pf f "ok -> %s" digest

let folder_digest_exn = function Some (_, Ok digest) -> digest | _ -> raise Not_found

let key_file path = Fpath.(state_dir // add_ext ".key" path)

let digest_file path = Fpath.(state_dir // add_ext ".sha256" path)

let get _ path =
  Bos.OS.File.read (key_file path)
  |> Result.to_option
  |> Option.map (fun key ->
         ( key,
           match Bos.OS.File.read (digest_file path) with
           | Ok v -> Ok (String.trim v)
           | Error _ -> Failed ))

let cmd_write_key key paths =
  let pp_write_key f folder =
    Fmt.pf f "mkdir -p cache/%a && echo '%s' > cache/%a.key" Fpath.pp (Fpath.parent folder) key
      Fpath.pp folder
  in
  Fmt.(str "%a" (list ~sep:(any " && ") pp_write_key) paths)

let cmd_compute_sha256 paths =
  let pp_compute_digest f folder =
    Fmt.pf f
      "(mkdir -p cache/%a && (find %a/ -type f -exec sha256sum {} \\;) | sort -k 2 | sha256sum > \
       cache/%a.sha256)"
      Fpath.pp (Fpath.parent folder) Fpath.pp folder Fpath.pp folder
  in
  Fmt.(str "%a" (list ~sep:(any " && ") pp_compute_digest) paths)

let cmd_sync_folder t =
  Fmt.str "rsync -avz cache %s:%s/" (Config.Ssh.host t) (Config.Ssh.storage_folder t)

module Op = struct
  type t = No_context

  let pp f _ = Fmt.pf f "remote cache"

  module Key = struct
    type t = Config.Ssh.t

    let digest = Config.Ssh.digest
  end

  module Value = Current.Unit

  let auto_cancel = true

  let id = id

  let build No_context job ssh =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Mostly_harmless job in
    let+ () = sync ~job ssh in 
    Result.Ok (())
end

module Cache = Current_cache.Make (Op)

let v ssh =
  let open Current.Syntax in
  let+ _ =
    Current.primitive
      ~info:(Current.component "remote cache pull")
      (Cache.get No_context) (Current.return ssh)
  in
  ssh
