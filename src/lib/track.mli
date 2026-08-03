type t [@@deriving yojson]

val digest : t -> string
val pkg : t -> OpamPackage.t

val v :
  repo_label:string ->
  limit:int option ->
  filter:string list ->
  Current_git.Commit.t Current.t ->
  (string * t list) Current.t
(** [repo_label] is a stable human-readable identifier for the repo — typically
    its filesystem path. It goes into the OCurrent component label so the same
    (filter, limit) can feed from multiple repos across one or more profiles
    without colliding.

    The result pairs the tracked packages with the {b commit hash} they were
    read from. The underlying op is latched: right after the repo moves, the
    current still carries the previous commit's list while re-tracking.
    Consumers that combine it with other repo-derived inputs must verify the
    embedded commit matches their view of the repo and treat mismatches as "not
    ready yet", or they will compute one evaluation on torn inputs. *)

val merge_values : t list list -> t list
(** Union per-repo tracking results with "later repos override" semantics, keyed
    by [(package_name, version)]. Mirrors opam's overlay resolution — an overlay
    repo that re-publishes the same [name.version] with modified content wins
    over mainline, while packages only in the overlay get added to the tracked
    universe. Sorted by package (the order feeds cache-key digests). *)

module Map : OpamStd.MAP with type key = t
