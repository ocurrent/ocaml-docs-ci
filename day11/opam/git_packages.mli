(** Package index from git commits.

    Reads opam packages directly from git tree objects without needing
    a working tree checkout. Supports lazy loading (packages read on
    demand) and eager loading (all at once). *)

module Store = Git_unix.Store

type t
(** A package index keyed by name, with lazy version maps. *)

val empty : t

val of_commit : ?super:t -> Store.t -> Store.Hash.t -> t
(** [of_commit ?super store commit] builds a package index from the
    [packages/] tree at [commit]. If [super] is provided, packages
    from [super] are included as a base (overridden by [commit]). *)

val of_commit_eager : Store.t -> Store.Hash.t -> t
(** [of_commit_eager store commit] reads all packages eagerly within
    a single Lwt run. Safer for use with Domains. *)

val of_opam_repository : string -> t * Store.t * Store.Hash.t
(** [of_opam_repository repo_path] opens the repo, reads HEAD, and
    returns the package index along with the store and commit hash. *)

val of_repositories : (string * string option) list ->
  t * (string * string) list
(** [of_repositories repos] loads packages from multiple repositories.
    Each element is [(repo_path, commit_sha_opt)]. Repositories are
    layered in order — later repos override earlier ones.
    Returns the merged package index and a list of
    [(repo_path, commit_sha_hex)] pairs for passing to workers. *)

val of_repositories_lwt : (string * string option) list ->
  (t * (string * string) list) Lwt.t
(** Lwt-native version of {!of_repositories}. Use this when calling
    from inside an already-running Lwt event loop (e.g. an OCurrent
    Op) to avoid nested {!Lwt_main.run} errors. *)

val force_all : t -> unit
(** [force_all t] forces all lazy version maps. Call before using
    [t] from multiple domains. *)

val get_versions :
  t -> OpamPackage.Name.t ->
  OpamFile.OPAM.t OpamPackage.Version.Map.t

val get_package : t -> OpamPackage.t -> OpamFile.OPAM.t

val find_package : t -> OpamPackage.t -> OpamFile.OPAM.t option
(** [find_package t pkg] returns [Some opam] if the package exists,
    [None] otherwise. *)

val all_names : t -> OpamPackage.Name.t list
(** [all_names t] returns all package names in the index. *)

val diff_packages :
  store:Store.t -> Store.Hash.t -> Store.Hash.t ->
  OpamPackage.Name.t list
(** [diff_packages ~store commit1 commit2] returns package names
    whose tree objects differ between the two commits.

    {b Asymmetric}: iterates [commit1]'s [packages/] entries, so it
    reports names changed in or removed from [commit1]'s view — a
    package present only in [commit2] (newly added) is not reported.
    Callers needing a symmetric diff should union both directions.

    Runs its own Lwt loop ([Lwt_main.run]); do not call from inside a
    running Lwt or Lwt_eio event loop — use {!diff_packages_lwt}. *)

val diff_packages_lwt :
  store:Store.t -> Store.Hash.t -> Store.Hash.t ->
  OpamPackage.Name.t list Lwt.t
(** Lwt-native version of {!diff_packages}, safe under a running
    event loop (e.g. from an OCurrent Op). *)
