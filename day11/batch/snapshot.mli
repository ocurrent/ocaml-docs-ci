(** Point-in-time state of all opam repositories within a profile.

    A snapshot captures the git commit SHA of each opam-repository at the time
    of a run. The snapshot key is a hash of all commit SHAs, used to identify
    which solver results and build outcomes belong together. Multiple runs may
    target the same snapshot (e.g. retries with [--rebuild-failed]). *)

type t = {
  repos : (string * string) list;
      (** [(repo_path, commit_sha)] for each opam-repository. *)
  key : string;  (** Hash of all commit SHAs — the snapshot identifier. *)
  created : string;
      (** ISO-8601 UTC timestamp of when the snapshot was first seen. *)
}

val current : Profile.t -> t
(** [current profile] reads the current HEAD of each opam-repository in
    [profile] and returns a snapshot. If a repo is not a git repository, uses
    ["unknown"] as the commit SHA. *)

val compute_key : (string * string) list -> string
(** [compute_key repos] hashes the commit SHAs into the 12-char snapshot key.
    Callers that already have [(path, sha)] pairs (e.g. from a live OCurrent
    poll) can use this directly rather than going through [current]. *)

val save : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [save dir snapshot] writes [repos.json] into [dir]. *)

val load : Fpath.t -> (t, [> Rresult.R.msg ]) result
(** [load dir] reads [repos.json] from [dir]. *)

(** {1 Derived paths within a snapshot directory} *)

val solutions_dir : Fpath.t -> Fpath.t
val packages_dir : Fpath.t -> Fpath.t
val runs_dir : Fpath.t -> Fpath.t
val status_json : Fpath.t -> Fpath.t
