open Config

let override = true

let ssh_run_prefix ssh =
  let remote = Ssh.user ssh ^ "@" ^ Ssh.host ssh in
  Bos.Cmd.(
    v "ssh" % "-o" % "StrictHostKeyChecking=no" % "-p"
    % (Ssh.port ssh |> string_of_int)
    % "-i"
    % p (Ssh.priv_key_file ssh)
    % remote)

let setup ssh =
  Log.app (fun f -> f "Checking storage server status..");
  let ensure_program program = Fmt.str "%s --version" program in
  let ensure_dir dir =
    let path = Fpath.(v (Ssh.storage_folder ssh) / dir) in
    Fmt.str "mkdir -p %a" Fpath.pp path
  in
  let ensure_git_repo ?(extra_branches = []) dir =
    let dir = Fpath.of_string dir |> Result.get_ok in
    let pp_git_update_ref_branch f = Fmt.pf f "git update-ref refs/heads/%s $COMMIT" in
    let pp_git_init_command f extra_branches =
      Fmt.pf f
        "git init --bare && echo 'ref: refs/heads/main' > HEAD && COMMIT=$(git commit-tree $(git \
         write-tree) -m 'root') && %a"
        Fmt.(list ~sep:(const string " && ") pp_git_update_ref_branch)
        ("main" :: extra_branches)
    in
    let path = Fpath.(v (Ssh.storage_folder ssh) // dir) in
    Fmt.str "mkdir -p %a && cd %a && (git rev-parse --git-dir || (%a))" Fpath.pp path Fpath.pp path
      pp_git_init_command extra_branches
  in
  let run cmd =
    let cmd = Bos.Cmd.(ssh_run_prefix ssh % cmd) in
    Bos.OS.Cmd.run cmd
  in

  if override then Ok ()
  else
    let ( let* ) = Result.bind in
    let ( let+ ) a b = Result.map b a in
    let* () = ensure_program "git" |> run in
    let* () = ensure_program "rsync" |> run in
    let* () = ensure_dir "prep" |> run in
    let* () = ensure_dir "compile" |> run in
    let* () = ensure_dir "linked" |> run in
    let* () = ensure_git_repo ~extra_branches:[ "live"; "status" ] "git/html-tailwind" |> run in
    let+ () = ensure_git_repo ~extra_branches:[ "live" ] "git/html-classic" |> run in
    Log.app (fun f -> f "..OK!")
