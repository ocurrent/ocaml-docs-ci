type repository = HtmlTailwind | HtmlClassic | Linked | Compile | Prep | Cache

module Branch = struct
  type t = string

  let v = Package.digest

  let to_string = Fun.id
end

let string_of_repository = function
  | HtmlTailwind -> "html-tailwind"
  | HtmlClassic -> "html-classic"
  | Linked -> "linked"
  | Compile -> "compile"
  | Prep -> "prep"
  | Cache -> "cache"

let all_repositories = [ HtmlTailwind; HtmlClassic; Compile; Linked; Prep; Cache ]

let remote repository t =
  Fmt.str "%s@%s:%s/git/%s" (Config.Ssh.user t) (Config.Ssh.host t) (Config.Ssh.storage_folder t)
    (string_of_repository repository)

let git_checkout_or_create b =
  Fmt.str
    "(git remote set-branches --add origin %s && git fetch origin %s && git checkout --track \
     origin/%s) || (git checkout -b %s && git push --set-upstream origin %s)"
    b b b b b

let branch_of_package p = Package.digest p

let print_branches_info ~prefix ~branches =
  let pp_print_branch_info f b =
    Fmt.pf f
      {|printf "%s:%s:$(git rev-parse %s):$(git cat-file -p %s | grep tree | cut -f2- -d' ')\n"|}
      prefix b b b
  in
  Fmt.to_to_string Fmt.(list ~sep:(const string "&&") pp_print_branch_info) branches

type branch_info = { branch : string; tree_hash : string; commit_hash : string }
[@@deriving yojson]

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

  let write_folder_command ~base ~message (branch, folder) =
    Fmt.str
      "(cd /tmp/git-store && (%s) && rm -rf content) && rsync -avzR %s/./%s \
       /tmp/git-store/content/ && (cd /tmp/git-store && git add --all && (git diff --quiet \
       --exit-code --cached || git commit -m '%s') && git checkout main)"
      (git_checkout_or_create branch) base folder message

  let write_folders_to_git ~repository ~ssh ~branches ~folder ~message ~git_path =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "git clone --single-branch %s %s && %s && (cd %s && git push --all -f)"
      (remote repository ssh) git_path
      (List.map (write_folder_command ~base:folder ~message) branches |> String.concat " && ")
      git_path

  let pull_to_directory ~repository ~ssh ~branches ~directory =
    Obuilder_spec.run ~network:[ "host" ] ~secrets:Config.Ssh.secrets
      "git clone --single-branch %s /tmp/git-store/ && (cd /tmp/git-store/ && git fetch origin %s \
       && git merge -m '.' %s && mkdir -p /tmp/git-store/content) && mv /tmp/git-store/content %s \
       && rm -rf /tmp/git-store"
      (remote repository ssh)
      (List.map (fun x -> x ^ ":" ^ x) branches |> String.concat " ")
      (branches |> String.concat " ")
      directory
end

module Local = struct
  let clone ~branch ~directory repository t =
    Bos.Cmd.(
      v "env"
      % Fmt.str "GIT_SSH_COMMAND=ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t)
          Fpath.pp (Config.Ssh.priv_key_file t)
      % "git" % "clone" % "--single-branch" % "-b" % branch % remote repository t % p directory)

  let push ~directory t =
    Bos.Cmd.(
      v "env"
      % Fmt.str "GIT_SSH_COMMAND=ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port t)
          Fpath.pp (Config.Ssh.priv_key_file t)
      % "git" % "-C" % p directory % "push")

end
