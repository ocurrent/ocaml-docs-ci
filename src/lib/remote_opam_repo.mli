(** Maintain a local clone of a remote opam-repository at a user-specified path.

    Kept separate from [Day11_batch.Profile] so the profile schema (and
    therefore [day11 batch]) stays local-paths-only. The day11 profile
    references [path]; this module keeps [path] fresh from [url]. *)

type spec = { url : string; path : Fpath.t }
(** One [--remote URL=PATH] entry: "clone {!field:url} into {!field:path},
    fetching periodically." *)

val spec_of_arg : string -> (spec, [> `Msg of string ]) result
(** Parse a [URL=PATH] CLI argument. Split on the first [=]. *)

val maintain :
  schedule:Current_cache.Schedule.t ->
  url:string ->
  path:Fpath.t ->
  string Current.t
(** [maintain ~schedule ~url ~path] installs an OCurrent job that:
    - on first run, [git clone --depth=1 URL PATH];
    - on each subsequent schedule fire, [git fetch + git merge --ff-only @{u}]
      in [PATH].

    Returns the latest commit SHA. *)

val maintain_commit :
  schedule:Current_cache.Schedule.t ->
  url:string ->
  path:Fpath.t ->
  Current_git.Commit.t Current.t
(** Same as {!maintain}, but lifts the SHA into a [Current_git.Commit.t] keyed
    by [path]. Downstream consumers that want a [Commit.t] should use this
    instead of pairing {!maintain} with [Current_git.Local.head_commit] — the
    latter relies on an inotify watcher on [path/.git/], which can no-op
    silently if the pull job claims success without actually advancing HEAD. *)
