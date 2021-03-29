let id = "digest-cache"

let state_dir = Current.state_dir id

let ssh_pool = Current.Pool.create ~label:"ssh" 30

let sync ~job () =
  let open Lwt.Syntax in
  let remote_folder = Fmt.str "%s@@%s:%s/" Config.ssh_user Config.ssh_host Config.storage_folder in
  let switch = Current.Switch.create ~label:"ssh" () in
  let* () = Current.Job.use_pool ~switch job ssh_pool in
  let* _ =
    Current.Process.exec ~cancellable:true ~job
      ( "",
        [|
          "rsync";
          "-avzR";
          "-e";
          Fmt.str "ssh -p %d -i %a" Config.ssh_port Fpath.pp Config.ssh_priv_key_file;
          remote_folder ^ "/digests/./";
          Fpath.to_string state_dir;
        |] )
  in
  Current.Switch.turn_off switch

type t = unit

let get () path =
  Bos.OS.File.read Fpath.(state_dir // add_ext ".sha256" path)
  |> Result.to_option |> Option.map String.trim

let compute_cmd paths =
  let pp_compute_digest f folder =
    Fmt.pf f
      "(mkdir -p digests/%a && (find %a/ -type f -exec sha256sum {} \\;) | sort -k 2 | sha256sum > \
       digests/%a.sha256)"
      Fpath.pp (Fpath.parent folder) Fpath.pp folder Fpath.pp folder
  in
  Fmt.(str "%a" (list ~sep:(any " && ") pp_compute_digest) paths)

module Op = struct
  type t = No_context

  let pp f _ = Fmt.pf f "digests"

  module Key = Current.Unit
  module Value = Current.Unit

  let auto_cancel = true

  let id = "digests"

  let build No_context job () =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Mostly_harmless job in
    let* () = sync ~job () in
    Lwt.return_ok ()
end

module Cache = Current_cache.Make (Op)

let v () =
  Current.primitive
    ~info:(Current.component "digests pull")
    (Cache.get No_context) (Current.return ())
