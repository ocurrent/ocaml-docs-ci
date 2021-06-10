type repository = HtmlTailwind | HtmlClassic | Linked | Compile | Prep

module Branch = struct
  type t = string

  let v p =
    Package.digest p
    |> String.map (function '~' | '^' | ':' | '\\' | '?' | '*' | '[' -> '-' | c -> c)

  let to_string = Fun.id
end

let string_of_repository = function
  | HtmlTailwind -> "html-tailwind"
  | HtmlClassic -> "html-classic"
  | Linked -> "linked"
  | Compile -> "compile"
  | Prep -> "prep"

let all_repositories = [ HtmlTailwind; HtmlClassic; Compile; Linked; Prep ]

let remote repository t =
  Fmt.str "%s@%s:%s/git/%s" (Config.Ssh.user t) (Config.Ssh.host t) (Config.Ssh.storage_folder t)
    (string_of_repository repository)

(* 1) try to checkout. 2) try to fetch from remote. 3) create new branch *)
let git_checkout_or_create b =
  Fmt.str
    "(git checkout %s) || (git remote set-branches --add origin %s && git fetch origin %s && git \
     checkout --track origin/%s) || (git checkout -b %s main && git push --set-upstream origin %s)"
    b b b b b b

let print_branches_info ~prefix ~branches =
  let pp_print_branch_info f b =
    Fmt.pf f
      {|printf "%s:%s:$(git rev-parse %s):$(git cat-file -p %s | grep tree | cut -f2- -d' ')\n"|}
      prefix b b b
  in
  Fmt.to_to_string Fmt.(list ~sep:(const string "&&") pp_print_branch_info) branches

type branch_info = { branch : string; tree_hash : string; commit_hash : string } [@@deriving yojson]

let parse_branch_info ~prefix line =
  match String.split_on_char ':' line with
  | [ prev; branch; commit_hash; tree_hash ] when Astring.String.is_suffix ~affix:prefix prev ->
      Some { branch; commit_hash; tree_hash }
  | _ -> None

module Cluster = struct
  let git_clone_command ~repository ~ssh ~branch directory =
    Fmt.str "git clone --single-branch %s %s && (cd %s && (%s))" (remote repository ssh) directory
      directory (git_checkout_or_create branch)

  let write_folder_to_git ~repository ~ssh ~branch ~folder ~message ~git_path =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "%s && rm -rf %s/content && mv %s %s/content && cd %s && git add --all && (git diff --quiet \
       --exit-code --cached || git commit -m '%s') && git push -f"
      (git_clone_command ~repository ~ssh ~branch git_path)
      git_path folder git_path git_path message

  let write_folder_command ~base ~message ~git_path (branch, folder) =
    Fmt.str
      "(cd %s && (%s) && rm -rf content) && rsync -avzR %s/./%s %s/content/ && (cd %s && git add \
       --all && (git diff --quiet --exit-code --cached || git commit -m '%s'))"
      git_path (git_checkout_or_create branch) base folder git_path git_path message

  let write_folders_to_git ~repository ~ssh ~branches ~folder ~message ~git_path =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "git clone --single-branch %s %s && (for DATA in %s; do IFS=\",\"; set -- $DATA; %s done) && \
       (cd %s && git push --all -f)"
      (remote repository ssh) git_path
      (List.map (fun (branch, folder) -> branch ^ "," ^ folder) branches |> String.concat " ")
      (write_folder_command ~base:folder ~message ~git_path ("$1", "$2"))
      git_path

  let pull_to_directory ~repository ~ssh ~branches ~directory =
    let commits = List.rev_map (fun (_, `Commit c) -> c) branches in
    let branches = List.rev_map fst branches in
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "git clone --single-branch %s /tmp/git-store/ && (cd /tmp/git-store/ && git fetch origin %s \
       && git merge -m '.' %s && mkdir -p /tmp/git-store/content) && mv /tmp/git-store/content %s \
       && rm -rf /tmp/git-store"
      (remote repository ssh)
      (List.map (fun x -> x ^ ":" ^ x) branches |> String.concat " ")
      (commits |> String.concat " ")
      directory
end

module Local = struct
  let env_prefix t =
    Bos.Cmd.(
      v "env"
      % Fmt.str "GIT_SSH_COMMAND=ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t)
          Fpath.pp (Config.Ssh.priv_key_file t))

  let clone ~branch ~directory repository t =
    Bos.Cmd.(
      env_prefix t % "git" % "clone" % "--single-branch" % "-b" % branch % remote repository t
      % p directory)

  let checkout_or_create ~branch t =
    Bos.Cmd.(env_prefix t % "bash" % "-c" % git_checkout_or_create branch)

  let push ~directory t = Bos.Cmd.(env_prefix t % "git" % "-C" % p directory % "push")
end
