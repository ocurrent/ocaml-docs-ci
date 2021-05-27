let remote t =
  Fmt.str "%s@%s:%s/git" (Config.Ssh.user t) (Config.Ssh.host t) (Config.Ssh.storage_folder t)

let git_checkout_or_create b =
  Fmt.str
    "(git remote set-branches --add origin %s && git fetch origin %s && git checkout --track \
     origin/%s) || (git checkout -b %s && git push --set-upstream origin %s)"
    b b b b b

module Cluster = struct
  let clone ~branch ~directory t =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "git clone --single-branch %s %s && cd %s && (%s)" (remote t) directory directory
      (git_checkout_or_create branch)

  let push ?(force = false) _ =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      (if force then "git push -f" else "git push")
end

module Local = struct
  let clone ~branch ~directory t =
    Bos.Cmd.(
      v "env"
      % Fmt.str "GIT_SSH_COMMAND=ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t)
          Fpath.pp (Config.Ssh.priv_key_file t)
      % "git" % "clone" % "--single-branch" % "-b" % branch % remote t % p directory)

  let push ~directory t =
    Bos.Cmd.(
      v "env"
      % Fmt.str "GIT_SSH_COMMAND=ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t)
          Fpath.pp (Config.Ssh.priv_key_file t)
      % "git" % "-C" % p directory % "push")
end
