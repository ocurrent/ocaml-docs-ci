(** Per-tick [Profile_ctx.t] loader.

    [Profile_ctx.load] calls [Git_packages.of_repositories] which internally
    uses [Lwt_main.run] via git-unix. Running that from inside OCurrent's Lwt
    engine (under [Lwt_eio.with_event_loop]) would nest event loops — undefined
    behaviour. We dodge that by detaching the load into a thread via
    [Lwt_preemptive.detach], so the git-unix code gets its own fresh Lwt world.

    [Profile_ctx.t] isn't marshalable (closures, hashtables), so the Op's
    [Value] is just a digest marker; the actual ctx is stashed in a per-profile
    [ref] and picked up by the caller after the Op resolves. Safe because
    [Current_cache] serialises [build] per-key, so the ref update for a given
    profile never races with itself. *)

module Profile = Day11_batch.Profile
module Profile_ctx = Day11_batch.Profile_ctx

(* Side-channel storage for the non-marshalable [Profile_ctx.t].
   The Op's build writes into [stores] keyed by the profile name;
   the resolving pipeline reads back from it. *)
let stores : (string, Profile_ctx.t) Hashtbl.t = Hashtbl.create 4

(* Per-process nonce — included in [Key.digest] so that every startup
   forces a cache miss, re-runs [build], and repopulates [stores].
   Without this, a sqlite-cached prior success would skip [build] but
   leave [stores] empty. *)
let process_nonce =
  Printf.sprintf "%d-%f" (Unix.getpid ()) (Unix.gettimeofday ())

module Op = struct
  type t = { profile : Profile.t; cache_dir : Fpath.t }

  module Key = struct
    type t = { profile_name : string; repos_with_shas : (string * string) list }

    let digest k =
      let sorted =
        List.sort compare
          (List.map (fun (p, s) -> p ^ "@" ^ s) k.repos_with_shas)
      in
      Digest.to_hex
        (Digest.string
           (String.concat "\n" (process_nonce :: k.profile_name :: sorted)))

    let pp f k =
      Fmt.pf f "profile-ctx %s (%d repos)" k.profile_name
        (List.length k.repos_with_shas)
  end

  module Value = struct
    (* The [Profile_ctx.t] isn't marshalable; the Op's value is just
       the key digest so [Current_cache] can detect re-runs. *)
    type t = string

    let marshal = Fun.id
    let unmarshal = Fun.id
  end

  let id = "day11-profile-ctx"
  let auto_cancel = false
  let pp f (key : Key.t) = Key.pp f key

  (* Snapshots live at [~/.day11/snapshots/<profile>/<key>/].
     [cache_dir] is [~/.day11/cache], so we go one up then down
     into [snapshots]. Matches the batch CLI's layout so that the
     [snapshots]/[diff]/[results] commands see docs-ci runs too. *)
  let snapshots_base_for ~cache_dir profile_name =
    Fpath.(parent cache_dir / "snapshots" / profile_name)

  let write_snapshot_and_run ~cache_dir ~profile_name ~repos_with_shas =
    let snapshots_base = snapshots_base_for ~cache_dir profile_name in
    let snapshot =
      let key = Day11_batch.Snapshot.compute_key repos_with_shas in
      let created =
        let t = Unix.gettimeofday () in
        let tm = Unix.gmtime t in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
          (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
          tm.Unix.tm_sec
      in
      Day11_batch.Snapshot.{ repos = repos_with_shas; key; created }
    in
    let dir = Fpath.(snapshots_base / snapshot.key) in
    ignore (Bos.OS.Dir.create ~path:true dir);
    let _ = Day11_batch.Snapshot.save dir snapshot in
    snapshot

  let build op job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    Current.Job.log job "[profile %s] loading ctx for %d repos" op.profile.name
      (List.length key.repos_with_shas);
    (* Override the profile's static [opam_repositories] with the
       live paths we've just resolved via Current_git. *)
    let profile =
      { op.profile with opam_repositories = List.map fst key.repos_with_shas }
    in
    let* ctx = Profile_ctx.load_lwt profile ~cache_dir:op.cache_dir in
    Hashtbl.replace stores op.profile.name ctx;
    let snapshot =
      write_snapshot_and_run ~cache_dir:op.cache_dir
        ~profile_name:op.profile.name ~repos_with_shas:key.repos_with_shas
    in
    Current.Job.log job "[profile %s] ctx loaded, snapshot=%s" op.profile.name
      snapshot.key;
    Lwt.return (Ok (Key.digest key))
end

module Cache = Current_cache.Make (Op)

(** Read the on-disk HEAD of a local git repo as a one-shot
    [Current_git.Commit.t Current.t]. Used as the fallback when an entry has no
    remote-pull job to drive it (e.g. read-only overlays maintained outside the
    daemon). The commit is captured once at pipeline-construction time and never
    changes — manual edits to the repo will require a daemon restart, which is
    the correct trade-off for a static overlay. *)
let one_shot_head_commit (path : Fpath.t) : Current_git.Commit.t Current.t =
  let p = Fpath.to_string path in
  let cmd = Printf.sprintf "git -C %s rev-parse HEAD" (Filename.quote p) in
  let ic = Unix.open_process_in cmd in
  let sha =
    try String.trim (input_line ic)
    with _ -> "0000000000000000000000000000000000000000"
  in
  ignore (Unix.close_process_in ic);
  Current.return
    (Current_git.Commit.v ~repo:path
       ~id:(Current_git.Commit_id.v ~repo:p ~gref:"refs/heads/master" ~hash:sha))

(** Resolve one [opam_repositories] entry to a live
    [Current_git.Commit.t Current.t].

    Entries are local paths. If the path is in [remote_commits] (i.e. a remote
    was declared via [--remote URL=PATH]), the commit comes directly from the
    {!Docs_ci_lib.Remote_opam_repo.maintain_commit} output for that remote — no
    inotify, no filesystem watcher; the pull job's post-fetch SHA flows straight
    through OCurrent. Local- only entries (no [--remote]) fall back to
    {!one_shot_head_commit}.

    The previous version watched every entry with [Current_git.Local], which
    decoupled solver triggers from pull-job outcomes — and silently masked bugs
    where the pull "succeeded" but didn't move HEAD (detached-HEAD, no upstream
    tracking, etc.). Reading the SHA from the pull job directly makes the
    dependency edge explicit. *)
let repo_commit ~remote_commits entry : Current_git.Commit.t Current.t =
  match Hashtbl.find_opt remote_commits entry with
  | Some c -> c
  | None -> one_shot_head_commit (Fpath.v entry)

(** A live [(path, sha) Current.t] for one entry of a profile's
    [opam_repositories]. Paths are observed via {!Current_git.Local} only —
    URL-based cloning lives in {!Docs_ci_lib.Remote_opam_repo}, kept separate so
    that the [Day11_batch.Profile] stays local-path-only across both
    ocaml-docs-ci and the [day11] CLI. *)
let repo_source ~remote_commits entry : (string * string) Current.t =
  let open Current.Syntax in
  let commit = repo_commit ~remote_commits entry in
  let+ commit = commit in
  let sha = Current_git.Commit_id.hash (Current_git.Commit.id commit) in
  (entry, sha)

(** The commits that drive package tracking for a profile.

    Returns one [Current_git.Commit.t Current.t] per entry in
    [profile.opam_repositories], in declaration order. Callers merge per-repo
    tracking results with "later entry wins" semantics, matching opam's overlay
    behaviour — so a profile's oxcaml overlay can both {e add} new packages
    (e.g. [oxcaml-compiler]) and {e override} mainline entries by name+version.
*)
let tracking_commits ~remote_commits (profile : Profile.t) =
  match profile.opam_repositories with
  | [] ->
      failwith
        (Printf.sprintf
           "profile %s: opam_repositories is empty — nothing to track"
           profile.name)
  | entries -> List.map (repo_commit ~remote_commits) entries

(** Full repos_with_shas [Current.t] for a profile. *)
let repos_with_shas ~remote_commits (profile : Profile.t) =
  let entries =
    List.map (repo_source ~remote_commits) profile.opam_repositories
  in
  Current.list_seq entries

(** The snapshot directory (on disk) for a given [(path, sha) list]. Used by
    callers that want to write per-snapshot outputs (status.json, solutions,
    etc.). *)
let snapshot_dir_of ~cache_dir ~profile_name repos_with_shas =
  let key = Day11_batch.Snapshot.compute_key repos_with_shas in
  Fpath.(Op.snapshots_base_for ~cache_dir profile_name / key)

type resolved = {
  ctx : Day11_batch.Profile_ctx.t Current.t;
  snapshot_dir : Fpath.t Current.t;
  repos_with_shas : (string * string) list Current.t;
  tracking_commits : Current_git.Commit.t Current.t list;
      (** One commit per entry in [profile.opam_repositories], in declaration
          order. Callers run [Track.v] per commit and merge with "later wins" so
          overlay repos contribute their own packages on top of mainline. *)
}
(** Bundle of everything a profile's sub-pipeline needs per tick. Returned by
    [resolve]. The [ctx] and [snapshot_dir] currents are driven by the same
    underlying [repos_with_shas] polling, so they change together on any
    opam-repo commit. *)

let resolve ~remote_commits ~cache_dir (profile : Profile.t) =
  let open Current.Syntax in
  let op = Op.{ profile; cache_dir } in
  let repos = repos_with_shas ~remote_commits profile in
  let digest =
    Current.component "[%s] profile-ctx" profile.name
    |>
    let> repos_with_shas = repos in
    Cache.get op Op.Key.{ profile_name = profile.name; repos_with_shas }
  in
  let ctx =
    let+ _digest = digest in
    match Hashtbl.find_opt stores profile.name with
    | Some ctx -> ctx
    | None ->
        failwith
          (Printf.sprintf "profile %s: ctx store unpopulated" profile.name)
  in
  let snapshot_dir =
    let+ repos_with_shas = repos in
    snapshot_dir_of ~cache_dir ~profile_name:profile.name repos_with_shas
  in
  let tracking_commits = tracking_commits ~remote_commits profile in
  { ctx; snapshot_dir; repos_with_shas = repos; tracking_commits }
