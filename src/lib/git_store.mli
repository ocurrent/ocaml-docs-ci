type repository = HtmlTailwind | HtmlClassic | Linked | Compile | Prep

val string_of_repository : repository -> string

val all_repositories : repository list

module Branch : sig
  type t

  val v : Package.t -> t

  val to_string : t -> string
end

module Cluster : sig
  val write_folder_to_git :
    repository:repository ->
    ssh:Config.Ssh.t ->
    branch:Branch.t ->
    folder:string ->
    message:string ->
    git_path:string ->
    Obuilder_spec.op

  val write_folders_to_git :
    repository:repository ->
    ssh:Config.Ssh.t ->
    branches:(Branch.t * string) list ->
    folder:string ->
    message:string ->
    git_path:string ->
    Obuilder_spec.op

  val pull_to_directory :
    repository:repository ->
    ssh:Config.Ssh.t ->
    branches:(Branch.t * [ `Commit of string ]) list ->
    directory:string ->
    Obuilder_spec.op
end

module Local : sig
  val clone : branch:string -> directory:Fpath.t -> repository -> Config.Ssh.t -> Bos.Cmd.t

  val push : directory:Fpath.t -> Config.Ssh.t -> Bos.Cmd.t

  val checkout_or_create : branch:string -> Config.Ssh.t -> Bos.Cmd.t
end

val remote : repository -> Config.Ssh.t -> string

val print_branches_info : prefix:string -> branches:Branch.t list -> string

type branch_info = { branch : string; tree_hash : string; commit_hash : string } [@@deriving yojson]

val parse_branch_info : prefix:string -> string -> branch_info option
