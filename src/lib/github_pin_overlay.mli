(** Maintain a synthetic opam-repository overlay built from a github URL.

    Tracks an upstream branch (the default branch, or an explicit [?branch]) of
    a github repo (e.g. [github.com/ocaml/odoc]) and republishes its [.opam]
    manifests as an overlay opam-repository on disk, with each package's [src:]
    rewritten to [git+https://…#<sha>] and [version:] set to
    [<latest-tag>+<branch>.<commit-epoch>.<sha7>] (the label after [+] is the
    tracked branch, or [master] for the default branch). The overlay is itself a
    git repo so day11's existing [Profile_ctx_loader] picks it up via the same
    [repos_with_shas] mechanism as a regular opam-repository [--remote].

    The on-disk layout under [path/]:
    {v
    upstream/   ← clone of <url>, fetched on each schedule tick
    repo/       ← generated overlay opam-repo (its own git repo)
      repo
      packages/<name>/<name>.<version>/opam
    v}

    Profiles reference [<path>/repo] in their [opam_repositories] list; commits
    in [<path>/repo] flow through to a re-plan via OCurrent the same way
    mainline opam-repository commits do. *)

type spec = { url : string; branch : string option; path : Fpath.t }
(** One [--github-pin-overlay URL[#BRANCH]=PATH] entry: track {!field:url}
    (optionally a non-default {!field:branch}) and keep an overlay under
    {!field:path}. *)

val spec_of_arg : string -> (spec, [> `Msg of string ]) result
(** Parse a [URL[#BRANCH]=PATH] CLI argument. Splits the spec on the first [=];
    an optional [#BRANCH] suffix on the URL portion (split on its last [#])
    selects a non-default branch to track. *)

val maintain :
  ?branch:string ->
  schedule:Current_cache.Schedule.t ->
  url:string ->
  path:Fpath.t ->
  unit ->
  string Current.t
(** [maintain ?branch ~schedule ~url ~path ()] installs an OCurrent job that on
    each schedule tick:
    - fetches [url] into [path/upstream/] (clones on first run, on [?branch]
      when given, else the default branch);
    - reads upstream HEAD's SHA and the latest reachable tag;
    - regenerates [path/repo/packages/<name>/<name>.<ver>/opam] for each tracked
      package, with [version:] set to [<tag>+<branch>.<commit-epoch>.<sha7>] and
      [src:] pointing at the pinned commit;
    - commits the overlay if anything changed.

    Returns the overlay's HEAD SHA after the run. Same SHA across no-op ticks;
    downstream therefore re-plans only on a real upstream change. *)

val maintain_commit :
  ?branch:string ->
  schedule:Current_cache.Schedule.t ->
  url:string ->
  path:Fpath.t ->
  unit ->
  Current_git.Commit.t Current.t
(** Same as {!maintain}, but returns a [Current_git.Commit.t] appropriate for
    stashing in [remote_commits] alongside the [--remote] entries. The returned
    commit's [hash] is the overlay's own SHA, not the upstream's — a profile
    referencing the overlay path picks up commits via the existing tracking
    machinery. *)
