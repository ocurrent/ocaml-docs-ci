let remote t = Fmt.str "%s@%s:%s/git" (Config.Ssh.user t) (Config.Ssh.host t) (Config.Ssh.storage_folder t)

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
