type t = string * Current_git.Commit_id.t
(** An opam repository and its name *)

type fetched = string * Current_git.Commit.t
(** A fetched opam repo *)

val pp : t Fmt.t

val compare : t -> t -> int

val fetch : t Current.t -> fetched Current.t

val unfetch : fetched -> t

val current_list_unfetch : fetched list Current.t -> t list Current.t
