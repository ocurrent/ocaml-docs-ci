module Log = Solver_api.Solver.Log

val open_store : unit -> Git_unix.Store.t Lwt.t
(** [open_store()] opens "./opam-repository" if it exists. If not fails an
    exception. *)

val clone : unit -> unit Lwt.t
(** [clone ()] ensures that "./opam-repository" exists. If not, it clones it. *)

val oldest_commit_with :
  log:Log.t -> from:Git_unix.Store.Hash.t -> OpamPackage.t list -> string Lwt.t
(** Use "git-log" to find the oldest commit with these package versions. This
    avoids invalidating the Docker build cache on every update to
    opam-repository.

    @param log The Capnp logger for this job.
    @param from The commit at which to begin the search. *)

val fetch : unit -> unit Lwt.t
(** Does a "git fetch origin" to update the store. *)
