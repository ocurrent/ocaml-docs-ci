module Op = struct
  type t = Config.Ssh.t * Current.Level.t

  module Key = struct
    type t = Fpath.t

    let digest = Fpath.to_string
  end

  module Value = Key
  module Outcome = Current.Unit

  let id = "symlink-folder"

  let pp f (k, v) = Fmt.pf f "Symlink folder: %a -> %a" Fpath.pp k Fpath.pp v

  let auto_cancel = true

  let publish (ssh, level) job name target_folder =
    let open Lwt.Syntax in
    let module Ssh = Config.Ssh in
    let* () = Current.Job.start ~level job in
    let live_file = Fpath.add_ext "log" name in
    let date_format = {|+"%Y-%m-%d %T"|} in
    let command =
      Bos.Cmd.(
        v "ssh" % "-p"
        % Int.to_string (Ssh.port ssh)
        % "-i"
        % p (Ssh.priv_key_file ssh)
        % (Ssh.user ssh ^ "@" ^ Ssh.host ssh)
        % Fmt.str "ln -sfT %a %a && echo `date %s` '%a' >> %a" Fpath.pp target_folder Fpath.pp name
            date_format Fpath.pp target_folder Fpath.pp live_file)
    in
    Current.Process.exec ~cancellable:true ~job ("", Bos.Cmd.to_list command |> Array.of_list)
end

module Publish = Current_cache.Output (Op)

let remote_symbolic_link ?(level = Current.Level.Dangerous) ~ssh ~target ~name () =
  Publish.set (ssh, level) name target
