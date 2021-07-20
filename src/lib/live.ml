module Op = struct
  type t = Config.Ssh.t

  module Key = Current.String

  module Value = struct
    type t = Epoch.t * [ `Html | `Linked ]

    let digest (v, k) = Epoch.digest k v
  end

  module Outcome = Current.Unit

  let id = "set-live-folder"

  let pp f (k, (v, _)) = Fmt.pf f "Set live folder for %s to %a" k Epoch.pp v

  let auto_cancel = true

  let publish ssh job name (generation, kind) =
    let open Lwt.Syntax in
    let module Ssh = Config.Ssh in
    let* () = Current.Job.start ~level:Dangerous job in
    let new_generation_folder = Storage.Base.generation_folder kind generation in
    let storage_folder = Fpath.(of_string (Ssh.storage_folder ssh) |> Result.get_ok) in
    let target_folder = Fpath.(storage_folder // new_generation_folder) in
    let live_folder = Fpath.(storage_folder / (name ^ "-live")) in
    let live_file = Fpath.add_ext "log" live_folder in
    let date_format = {|+"%Y-%m-%d %T"|} in
    let command =
      Bos.Cmd.(
        v "ssh" % "-p"
        % Int.to_string (Ssh.port ssh)
        % "-i"
        % p (Ssh.priv_key_file ssh)
        % (Ssh.user ssh ^ "@" ^ Ssh.host ssh)
        % Fmt.str "ln -sfT %a %a && echo `date %s` '%a' >> %a" Fpath.pp target_folder Fpath.pp
            live_folder date_format Fpath.pp new_generation_folder Fpath.pp live_file)
    in
    Current.Process.exec ~cancellable:true ~job ("", Bos.Cmd.to_list command |> Array.of_list)
end

module Publish = Current_cache.Output (Op)

let set_to ~ssh name kind value =
  let open Current.Syntax in
  Current.component "Set live folder"
  |> let> value = value in
     Publish.set ssh name (value, kind)
