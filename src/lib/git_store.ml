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

  let merge_to_live ~job ~ssh ~branch ~msg =
    (* this piece of magic invocations create a merge commit in the 'live' branch *)
    let live_ref = "refs/heads/live" in
    let update_ref = "refs/heads/" ^ branch in
    (* find nearest common ancestor of the two trees *)
    let git_merge_base = Fmt.str "git merge-base %s %s" live_ref update_ref in
    (* perform an aggressive merge *)
    let git_merge_trees =
      Fmt.str
        "git read-tree --empty && git read-tree -mi --aggressive $(%s) %s %s && git merge-index \
         ~/git-take-theirs.sh -a"
        git_merge_base live_ref update_ref
    in
    (* create a commit object using the newly created tree *)
    let git_commit_tree =
      Fmt.str "git commit-tree $(git write-tree) -p %s -p %s -m 'update %s'" live_ref update_ref msg
    in
    (* update the live branch *)
    let git_update_ref = Fmt.str "git update-ref %s $(%s)" live_ref git_commit_tree in

    Current.Process.exec ~cancellable:false ~job
      ( "",
        [|
          "ssh";
          "-i";
          Fpath.to_string (Config.Ssh.priv_key_file ssh);
          "-p";
          Config.Ssh.port ssh |> string_of_int;
          Fmt.str "%s@%s" (Config.Ssh.user ssh) (Config.Ssh.host ssh);
          Fmt.str "cd %s/git && %s && %s" (Config.Ssh.storage_folder ssh) git_merge_trees
            git_update_ref;
        |] )
end
