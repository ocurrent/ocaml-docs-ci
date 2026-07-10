(* See .mli for high-level description. *)

(* Packages from the upstream root we publish into the overlay.
   [odoc-bench] is excluded — it pulls in benchmarking deps that
   aren't needed for the doc-tooling closure. *)
let opam_files_to_publish = [
  "odoc"; "odoc-driver"; "odoc-md"; "odoc-parser"; "sherlodoc"
]

module Op = struct
  type t = unit

  module Key = struct
    type t = { url : string; branch : string option; path : Fpath.t }
    let digest t =
      t.url ^ "|" ^ (Option.value ~default:"" t.branch)
      ^ "|" ^ Fpath.to_string t.path
  end

  module Value = struct
    (* The overlay's HEAD SHA after regeneration. Same value across
       no-op ticks. *)
    type t = string
    let marshal t = t
    let unmarshal t = t
  end

  let id = "docs-ci-github-pin-overlay"
  let auto_cancel = false
  let pp f (key : Key.t) =
    Fmt.pf f "github-pin-overlay %s → %a" key.url Fpath.pp key.path

  let sh_log job fmt =
    Fmt.kstr (fun s ->
      Current.Job.log job "$ %s" s;
      let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
      let open Lwt.Syntax in
      let* output = Lwt_process.pread_lines cmd
        |> Lwt_stream.to_list in
      List.iter (Current.Job.log job "%s") output;
      Lwt.return (Ok (String.concat "\n" output))
    ) fmt

  let run_sh job fmt =
    Fmt.kstr (fun s ->
      Current.Job.log job "$ %s" s;
      let cmd = ("", [| "/bin/sh"; "-c"; s |]) in
      let open Lwt.Syntax in
      let* status = Lwt_process.exec cmd in
      match status with
      | Unix.WEXITED 0 -> Lwt.return (Ok ())
      | Unix.WEXITED n ->
        Lwt.return (Error (`Msg (Printf.sprintf
          "command exited %d: %s" n s)))
      | _ ->
        Lwt.return (Error (`Msg ("command signalled: " ^ s)))
    ) fmt

  let upstream_dir path = Fpath.(path / "upstream")
  let overlay_dir  path = Fpath.(path / "repo")

  let ensure_upstream_clone job ~url ~branch ~upstream =
    let p = Fpath.to_string upstream in
    let git_dir = Filename.concat p ".git" in
    let ( let** ) = Lwt_result.bind in
    if Sys.file_exists git_dir then begin
      let** _ = sh_log job "git -C %s fetch --prune --tags origin" p in
      (* Same dance as Remote_opam_repo: explicit detached-HEAD guard
         around [merge --ff-only @{u}], built via Printf because Fmt
         eats [@{u}] as a semantic-tag. The clone below already put us
         on the tracked [branch] (when set), so [@{u}] resolves to
         [origin/<branch>] and the fetch+merge advances it. *)
      let cmd = Printf.sprintf
        "git -C %s symbolic-ref -q HEAD >/dev/null && \
         git -C %s merge --ff-only '@{u}' || { \
           echo \"WARNING: HEAD detached at $(git -C %s rev-parse \
             --short HEAD); not auto-advancing.\" >&2; \
           true; }"
        p p p in
      let** _ = sh_log job "%s" cmd in
      Lwt.return (Ok ())
    end else
      match branch with
      | Some b ->
        run_sh job "git clone -b %s %s %s"
          (Filename.quote b) (Filename.quote url) (Filename.quote p)
      | None ->
        run_sh job "git clone %s %s"
          (Filename.quote url) (Filename.quote p)

  let read_head_sha job ~upstream =
    sh_log job "git -C %s rev-parse HEAD" (Fpath.to_string upstream)

  let read_latest_tag job ~upstream =
    (* Latest reachable annotated/lightweight tag. Falls back to
       [0.0.0] when the repo has no tags at all so the version
       string still parses. *)
    sh_log job
      "git -C %s describe --tags --abbrev=0 2>/dev/null || echo 0.0.0"
      (Fpath.to_string upstream)

  (* Commit-time epoch (UTC). Monotone per-commit and big enough
     (10 digits as of 2026) that opam's segment-numeric comparison
     orders versions correctly: opam parses leading-digit runs as
     numbers, so an older [%Y%m%d] date — at most 8 digits — still
     beats a hex sha that happens to start with a high digit (e.g.
     [27216c7] parses as [27] and beats [5e9c5c0] which parses as
     [5]). Using [%ct] (a 10-digit epoch) gives every newer commit
     a strictly larger numeric prefix. *)
  let read_commit_epoch job ~upstream ~sha =
    sh_log job "git -C %s show -s --format=%%ct %s"
      (Fpath.to_string upstream) sha

  (* Build the [git+https://…#sha] URL for opam to clone. *)
  let pinned_url ~upstream_url ~sha =
    let normalised =
      if String.length upstream_url >= 4
         && String.sub upstream_url 0 4 = "git+"
      then upstream_url
      else "git+" ^ upstream_url
    in
    OpamUrl.of_string (Printf.sprintf "%s#%s" normalised sha)

  (* Rewrite one upstream [.opam] file into the overlay package dir. *)
  let publish_one ~upstream ~overlay ~url_obj ~version_str name =
    let upstream_opam = Fpath.(upstream / (name ^ ".opam")) in
    if not (Bos.OS.File.exists upstream_opam |> Result.value ~default:false)
    then ()
    else begin
      let opam =
        let f = OpamFile.make
          (OpamFilename.raw (Fpath.to_string upstream_opam)) in
        OpamFile.OPAM.read f
      in
      let version = OpamPackage.Version.of_string version_str in
      let opam = OpamFile.OPAM.with_version version opam in
      let opam = OpamFile.OPAM.with_url url_obj opam in
      let target_dir =
        Fpath.(overlay / "packages" / name / (name ^ "." ^ version_str))
      in
      Bos.OS.Dir.create ~path:true target_dir |> ignore;
      let target = Fpath.(target_dir / "opam") in
      let f = OpamFile.make
        (OpamFilename.raw (Fpath.to_string target)) in
      OpamFile.OPAM.write f opam
    end

  let regenerate_overlay ~upstream ~overlay ~upstream_url ~sha
      ~version_str =
    Bos.OS.Dir.create ~path:true Fpath.(overlay / "packages") |> ignore;
    let url_obj =
      OpamFile.URL.create (pinned_url ~upstream_url ~sha) in
    List.iter (publish_one ~upstream ~overlay ~url_obj ~version_str)
      opam_files_to_publish;
    (* Repo-level manifest. Written once; a 2.0 stub is enough for
       opam to recognise it as a valid repository root. *)
    let repo_file = Fpath.(overlay / "repo") in
    if not (Bos.OS.File.exists repo_file |> Result.value ~default:false) then
      ignore (Bos.OS.File.write repo_file
                "opam-version: \"2.0\"\nbrowse: \"\"\nupstream: \"\"\n")

  let ensure_overlay_repo job ~overlay =
    let p = Fpath.to_string overlay in
    let git_dir = Filename.concat p ".git" in
    let ( let** ) = Lwt_result.bind in
    if Sys.file_exists git_dir then Lwt.return (Ok ())
    else begin
      let** () = run_sh job "git init -q -b master %s" (Filename.quote p) in
      let** () = run_sh job
        "git -C %s config user.email 'docs-ci@invalid'" (Filename.quote p) in
      let** () = run_sh job
        "git -C %s config user.name 'docs-ci'" (Filename.quote p) in
      Lwt.return (Ok ())
    end

  let commit_if_dirty job ~overlay ~sha ~tag ~version_str =
    let p = Fpath.to_string overlay in
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* status = sh_log job "git -C %s status --porcelain" p in
    match status with
    | Error _ as e -> Lwt.return e
    | Ok s when String.trim s = "" ->
      Current.Job.log job "overlay clean, no commit" ;
      Lwt.return (Ok ())
    | Ok _ ->
      let** () = run_sh job "git -C %s add -A" p in
      let msg = Printf.sprintf "Track %s @ %s (version %s)"
                  tag sha version_str in
      run_sh job "git -C %s commit -q -m %s" p (Filename.quote msg)

  let build () job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Harmless in
    let upstream = upstream_dir key.path in
    let overlay = overlay_dir key.path in
    Bos.OS.Dir.create ~path:true key.path |> ignore;
    Bos.OS.Dir.create ~path:true upstream |> ignore;
    Bos.OS.Dir.create ~path:true overlay |> ignore;
    let ( let** ) = Lwt_result.bind in
    let result =
      let** () =
        ensure_upstream_clone job ~url:key.url ~branch:key.branch ~upstream in
      let** sha_raw = read_head_sha job ~upstream in
      let sha = String.trim sha_raw in
      let** tag_raw = read_latest_tag job ~upstream in
      let tag = String.trim tag_raw in
      let** epoch_raw = read_commit_epoch job ~upstream ~sha in
      let epoch = String.trim epoch_raw in
      let sha7 =
        if String.length sha >= 7 then String.sub sha 0 7 else sha in
      (* Version label after the [+]: the tracked branch (default
         branch ⇒ "master"). Sanitised — opam versions allow only
         [A-Za-z0-9-._+~], so a branch like [feature/x] becomes
         [feature-x]. *)
      let label = match key.branch with
        | None -> "master"
        | Some b ->
          String.map (fun c ->
            match c with
            | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '.' | '~' -> c
            | _ -> '-') b
      in
      let version_str =
        Printf.sprintf "%s+%s.%s.%s" tag label epoch sha7 in
      Current.Job.log job
        "upstream %s @ %s (tag %s) → version %s"
        key.url sha tag version_str;
      regenerate_overlay
        ~upstream ~overlay
        ~upstream_url:key.url ~sha ~version_str;
      let** () = ensure_overlay_repo job ~overlay in
      let** () =
        commit_if_dirty job ~overlay ~sha ~tag ~version_str in
      sh_log job "git -C %s rev-parse HEAD" (Fpath.to_string overlay)
    in
    Lwt_result.map (fun out ->
      let sha = String.trim out in
      Current.Job.log job "%a @ %s" Fpath.pp overlay sha;
      sha) result
end

module Cache = Current_cache.Make (Op)

(* [Current.cutoff]: the overlay's HEAD sha is unchanged across no-op
   ticks (see [Value] above), but each scheduled rebuild returns a
   physically new string, which the default [(==)] propagation
   equality treats as a change — re-firing the solve → doc-plan chain
   hourly for nothing. See {!Docs_ci_lib.Remote_opam_repo.maintain}
   for the mechanics. *)
let maintain ?branch ~schedule ~url ~path () : string Current.t =
  let open Current.Syntax in
  (Current.component "github-pin-overlay %s%s" url
    (match branch with Some b -> "#" ^ b | None -> "") |>
   let> () = Current.return () in
   Cache.get ~schedule () Op.Key.{ url; branch; path })
  |> Current.cutoff ~eq:String.equal

let maintain_commit ?branch ~schedule ~url ~path () :
    Current_git.Commit.t Current.t =
  let open Current.Syntax in
  let+ sha = maintain ?branch ~schedule ~url ~path () in
  let overlay = Op.overlay_dir path in
  let p = Fpath.to_string overlay in
  Current_git.Commit.v ~repo:overlay
    ~id:(Current_git.Commit_id.v
           ~repo:p ~gref:"refs/heads/master" ~hash:sha)

type spec = { url : string; branch : string option; path : Fpath.t }

let spec_of_arg s =
  match String.index_opt s '=' with
  | None ->
    Error (`Msg (Printf.sprintf
      "--github-pin-overlay %S: expected URL[#BRANCH]=PATH" s))
  | Some i ->
    let url_part = String.sub s 0 i in
    let path = String.sub s (i + 1) (String.length s - i - 1) in
    (* An optional [#BRANCH] suffix on the URL selects a non-default
       branch to track (e.g. a fork's feature branch). Split on the
       last [#] so the rest of the URL is unaffected. *)
    let url, branch = match String.rindex_opt url_part '#' with
      | Some j ->
        let b = String.sub url_part (j + 1)
                  (String.length url_part - j - 1) in
        (String.sub url_part 0 j, if b = "" then None else Some b)
      | None -> (url_part, None)
    in
    if url = "" || path = "" then
      Error (`Msg (Printf.sprintf
        "--github-pin-overlay %S: URL and PATH must both be non-empty" s))
    else
      Ok { url; branch; path = Fpath.v path }
