(** Git repository access.

    Opens git stores and resolves commits/refs. Uses Lwt internally but exposes
    a synchronous API. *)

module Store = Git_unix.Store

val get_git_repo_store_and_hash : string -> Store.t * Store.Hash.t
(** [get_git_repo_store_and_hash repo_path] opens the git store at [repo_path]
    and resolves HEAD to a commit hash. *)

val get_git_repo_store_and_hash_commit :
  string -> string option -> Store.t * Store.Hash.t
(** [get_git_repo_store_and_hash_commit repo_path commit_opt] opens the store
    and resolves [commit_opt] (a ref name or hex SHA). If [None], resolves HEAD.
*)

val resolve_commit_in_store : Store.t -> string option -> Store.Hash.t
(** [resolve_commit_in_store store commit_opt] resolves a commit ref or SHA in
    an already-opened store. *)

val get_git_repo_store_and_hash_lwt : string -> (Store.t * Store.Hash.t) Lwt.t
(** Lwt-native version of {!get_git_repo_store_and_hash}. *)

val get_git_repo_store_and_hash_commit_lwt :
  string -> string option -> (Store.t * Store.Hash.t) Lwt.t
(** Lwt-native version of {!get_git_repo_store_and_hash_commit}. *)

val resolve_commit_in_store_lwt : Store.t -> string option -> Store.Hash.t Lwt.t
(** Lwt-native version of {!resolve_commit_in_store}. *)
