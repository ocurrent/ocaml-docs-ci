val oldest_commit_with :
  from:Current_git.Commit.t -> OpamPackage.t list -> string Lwt.t
(** Use "git-log" to find the oldest commit with these package versions. This
    avoids invalidating the Docker build cache on every update to
    opam-repository.

    @param from The commit at which to begin the search. *)
