open Config

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
  let ensure_dir dir =
    let path = Fpath.(v (Ssh.storage_folder ssh) / dir) in
    Fmt.str "mkdir -p %a" Fpath.pp path
  in
  let ensure_program program = Fmt.str "%s --version" program in
  let ensure_git_repo dir =
    let git_init_command =
      "git init --bare && \
      echo 'ref: refs/heads/main' > HEAD && \
      COMMIT=$(git commit-tree $(git write-tree) -m 'root') && \
      git update-ref refs/heads/main $COMMIT && \
      git update-ref refs/heads/live $COMMIT"
    in
    let path = Fpath.(v (Ssh.storage_folder ssh) / dir) in
    Fmt.str "cd %a && (git rev-parse --git-dir || (%s))" Fpath.pp path git_init_command
  in
  let run cmd =
    let cmd = Bos.Cmd.(ssh_run_prefix ssh % cmd) in
    Bos.OS.Cmd.run cmd
  in

  let ( let* ) = Result.bind in
  let ( let+ ) a b = Result.map b a in
  let* () = ensure_dir "git" |> run in
  let* () = ensure_dir "cache" |> run in
  let* () = ensure_dir "compile" |> run in
  let* () = ensure_dir "prep" |> run in
  let* () = ensure_program "git" |> run in
  let* () = ensure_program "rsync" |> run in
  let+ () = ensure_git_repo "git" |> run in
  Log.app (fun f -> f "..OK!")
