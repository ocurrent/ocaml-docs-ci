(** Maintain a local clone of a remote opam-repository at a user-specified path.

    ocaml-docs-ci reads a list of [{url, path}] pairs from [remotes.json] and
    installs one of these jobs per pair. The job performs a [git clone] the
    first time, and on every subsequent schedule tick
    [git fetch + reset --hard origin/HEAD] — so [path] is always a fresh mirror
    of the remote. Day11 profiles then reference [path] as a regular local-path
    entry; a [Current_git.Local] watcher elsewhere in the pipeline picks up the
    updated HEAD via inotify and re-triggers downstream work.

    Keeping this separate from [Day11_batch.Profile] means the profile schema
    stays local-paths-only, and [day11 batch] works off the same profile files
    without ever handling URLs. *)

module Op = struct
  type t = unit

  module Key = struct
    type t = { url : string; path : Fpath.t }

    let digest t = t.url ^ "|" ^ Fpath.to_string t.path
  end

  module Value = struct
    (* Resolved commit SHA after the pull. *)
    type t = string

    let marshal t = t
    let unmarshal t = t
  end

  let id = "docs-ci-remote-opam-repo"
  let auto_cancel = false
  let pp f (key : Key.t) = Fmt.pf f "pull %s → %a" key.url Fpath.pp key.path

  let sh_log job fmt =
    Fmt.kstr
      (fun s ->
        Current.Job.log job "$ %s" s;
        let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
        let open Lwt.Syntax in
        let* output = Lwt_process.pread_lines cmd |> Lwt_stream.to_list in
        List.iter (Current.Job.log job "%s") output;
        Lwt.return (Ok (String.concat "\n" output)))
      fmt

  let run_sh job fmt =
    Fmt.kstr
      (fun s ->
        Current.Job.log job "$ %s" s;
        let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
        let open Lwt.Syntax in
        let* status = Lwt_process.exec cmd in
        match status with
        | Unix.WEXITED 0 -> Lwt.return (Ok ())
        | Unix.WEXITED n ->
            Lwt.return
              (Error (`Msg (Printf.sprintf "command exited %d: %s" n s)))
        | _ -> Lwt.return (Error (`Msg ("command signalled: " ^ s))))
      fmt

  let build () job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Harmless in
    let path = Fpath.to_string key.path in
    let ( let** ) = Lwt_result.bind in
    let result =
      let git_dir = Filename.concat path ".git" in
      let** () =
        if Sys.file_exists git_dir then
          (* Non-destructive sync. The previous version used [git merge
             --ff-only FETCH_HEAD], which silently noops when HEAD is
             detached: fetch updates [origin/*] tracking refs and writes
             FETCH_HEAD with every fetched branch tagged
             [not-for-merge], so the merge step has nothing to merge
             and exits 0 without advancing HEAD. The repo then reports
             a stale SHA forever, and downstream solver/build state
             stays frozen even though origin has new commits.
             We detect the detached case explicitly and shell out to
             [git symbolic-ref] / [merge --ff-only @{u}] only when
             there is actually a tracking upstream to fast-forward
             against. The shell string is built with [Printf.sprintf]
             rather than passed straight to {!run_sh}'s Fmt format,
             because Fmt treats [@{...}] as a semantic-tag command
             and would strip the [@{u}] git revspec from our command. *)
          let** () = run_sh job "git -C %s fetch --prune origin" path in
          let cmd =
            Printf.sprintf
              "git -C %s symbolic-ref -q HEAD >/dev/null && git -C %s merge \
               --ff-only '@{u}' || { echo \"WARNING: HEAD detached at $(git -C \
               %s rev-parse --short HEAD); not auto-advancing. Run 'git -C %s \
               checkout master' to recover.\" >&2; true; }"
              path path path path
          in
          run_sh job "%s" cmd
        else
          run_sh job "git clone %s %s" (Filename.quote key.url)
            (Filename.quote path)
      in
      sh_log job "git -C %s rev-parse HEAD" path
    in
    Lwt_result.map
      (fun output ->
        let sha = String.trim output in
        Current.Job.log job "%a @ %s" Fpath.pp key.path sha;
        sha)
      result
end

module Cache = Current_cache.Make (Op)

(** [maintain ~schedule ~url ~path] installs a scheduled puller that keeps
    [path] up to date with [url]. Returns the latest commit SHA as a
    [Current.t].

    The result is wrapped in [Current.cutoff ~eq:String.equal]: a scheduled
    re-pull that resolves to the {e same} SHA must not re-fire downstream.
    Without the cutoff, each hourly refresh completes with a
    freshly-unmarshalled (physically new) sha string, the default [(==)]
    equality in OCurrent's propagation fails, and the whole track → solve →
    profile-ctx → doc-plan chain re-runs on every poll even when nothing moved.
    Errors still propagate ([Dyn.equal] never equates Ok with Error), so a
    failing pull remains visible in the pipeline. *)
let maintain ~schedule ~url ~path : string Current.t =
  let open Current.Syntax in
  Current.component "pull %s" url
  |> (let> () = Current.return () in
      Cache.get ~schedule () Op.Key.{ url; path })
  |> Current.cutoff ~eq:String.equal

(** Same as {!maintain}, but lifts the SHA into a
    [Current_git.Commit.t Current.t]. Downstream consumers that want to read the
    commit's tree should use this instead of pairing {!maintain} with
    [Current_git.Local.head_commit] — the latter relies on an inotify watcher on
    [path/.git/], which can no-op silently if the pull job claims success
    without actually advancing HEAD. With this function the post-pull SHA flows
    directly through OCurrent so a no-op pull means the same [Commit.t]
    downstream, no kernel watcher in the loop. *)
let maintain_commit ~schedule ~url ~path : Current_git.Commit.t Current.t =
  let open Current.Syntax in
  let+ sha = maintain ~schedule ~url ~path in
  Current_git.Commit.v ~repo:path
    ~id:(Current_git.Commit_id.v ~repo:url ~gref:"refs/heads/master" ~hash:sha)

type spec = { url : string; path : Fpath.t }
(** One remote-mirror entry, passed via [--remote URL=PATH]. *)

(** Parse a [URL=PATH] argument. The [=] separator is the first one in the
    string, so URLs containing [=] survive (paths shouldn't — but if they do,
    quote them per shell rules). *)
let spec_of_arg s =
  match String.index_opt s '=' with
  | None -> Error (`Msg (Printf.sprintf "--remote %S: expected URL=PATH" s))
  | Some i ->
      let url = String.sub s 0 i in
      let path = String.sub s (i + 1) (String.length s - i - 1) in
      if url = "" || path = "" then
        Error
          (`Msg
             (Printf.sprintf "--remote %S: URL and PATH must both be non-empty"
                s))
      else Ok { url; path = Fpath.v path }
