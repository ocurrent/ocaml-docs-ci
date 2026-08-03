(** Start-of-day environment checks for the ocaml-docs-ci daemon.

    Runs once at process startup, logs anything that would prevent or degrade
    normal operation, and never raises. Each check writes [Logs.app] for the OK
    case and [Logs.warn] / [Logs.err] for problems — picking the level by
    whether the daemon will limp on (warn) or behave incorrectly (err).

    Visible via [docker-compose logs daemon] (or [docker logs <id>], or the
    json-file at [/var/lib/docker/containers/<id>/<id>-json.log]).

    Why so many checks: the things that bite are mostly environmental
    (bind-mount ownership, missing helper binary, an inaccessible
    /var/run/docker.sock) and the daemon manifests them deep in a
    runc/git/sqlite call several layers down. Surfacing them up front saves a
    debugging round-trip. *)

let src =
  Logs.Src.create "ocaml-docs-ci.startup"
    ~doc:"Startup diagnostics for the ocaml-docs-ci daemon"

module Log = (val Logs.src_log src)

(* ── helpers ─────────────────────────────────────────────────── *)

let stat_owner path =
  try
    let st = Unix.stat path in
    Some (st.Unix.st_uid, st.Unix.st_gid)
  with _ -> None

let writable path =
  try
    Unix.access path [ Unix.W_OK ];
    true
  with Unix.Unix_error _ -> false

let exec_present prog =
  (* Walk PATH manually rather than [Sys.command "which"] because
     [which] isn't installed in every minimal image. *)
  let path = try Sys.getenv "PATH" with Not_found -> "" in
  let candidates = String.split_on_char ':' path in
  List.exists
    (fun dir ->
      let p = Filename.concat dir prog in
      try
        Unix.access p [ Unix.X_OK ];
        true
      with _ -> false)
    candidates

let exec_version prog args =
  let cmd =
    Printf.sprintf "%s %s 2>&1" (Filename.quote prog) (String.concat " " args)
  in
  try
    let ic = Unix.open_process_in cmd in
    let line = try Some (input_line ic) with End_of_file -> None in
    (* Drain rest so close_process_in gets a clean exit code. *)
    (try
       while true do
         ignore (input_line ic)
       done
     with End_of_file -> ());
    match Unix.close_process_in ic with Unix.WEXITED 0 -> line | _ -> None
  with _ -> None

let disk_free_gib path =
  (* statvfs gives free blocks; multiply by f_frsize. Wrapping in a
     [Sys.command] dodge so this file stays dependency-free of
     extunix etc.; awk does the maths. *)
  let cmd =
    Printf.sprintf
      "df -BG --output=avail %s 2>/dev/null | tail -1 | tr -dc '0-9'"
      (Filename.quote path)
  in
  try
    let ic = Unix.open_process_in cmd in
    let line = try input_line ic with End_of_file -> "" in
    ignore (Unix.close_process_in ic);
    try Some (int_of_string (String.trim line)) with _ -> None
  with _ -> None

(* ── checks ──────────────────────────────────────────────────── *)

let check_identity () =
  let uid = Unix.getuid () and gid = Unix.getgid () in
  let euid = Unix.geteuid () and egid = Unix.getegid () in
  let user = try (Unix.getpwuid uid).Unix.pw_name with Not_found -> "?" in
  Log.app (fun f ->
      f "running as uid=%d gid=%d (user=%s, euid=%d egid=%d)" uid gid user euid
        egid)

let check_path_writable ~kind path =
  let path_s = Fpath.to_string path in
  let uid = Unix.getuid () and gid = Unix.getgid () in
  match stat_owner path_s with
  | None ->
      (* Not a fatal error per se — daemon will try to create it lazily.
       But warn so the first creation attempt isn't a surprise. *)
      Log.warn (fun f ->
          f "%s %a: does not exist (will be created on first use)" kind Fpath.pp
            path)
  | Some (o_uid, o_gid) ->
      if writable path_s then
        Log.app (fun f ->
            f "%s %a: ok (owner=%d:%d, writable)" kind Fpath.pp path o_uid o_gid)
      else
        Log.err (fun f ->
            f
              "%s %a: NOT WRITABLE — daemon runs as %d:%d but path is %d:%d. \
               Fix with: sudo chown -R %d:%d %a"
              kind Fpath.pp path uid gid o_uid o_gid uid gid Fpath.pp path)

let check_helper ~name ~kind ~version_args ~severity =
  if exec_present name then
    match exec_version name version_args with
    | Some line -> Log.app (fun f -> f "%s %s: ok (%s)" kind name line)
    | None ->
        Log.warn (fun f ->
            f "%s %s: on PATH but failed to produce a version line" kind name)
  else
    match severity with
    | `Required ->
        Log.err (fun f ->
            f "%s %s: NOT FOUND on PATH — daemon will fail when it needs this"
              kind name)
    | `Optional ->
        Log.warn (fun f ->
            f "%s %s: not found on PATH (some features will be degraded)" kind
              name)

let check_docker_socket () =
  (* Used by [resolve_base_digest] via [docker manifest inspect]. The
     CLI alone is enough for registry-only operations; the socket is
     needed for anything that actually contacts the engine. *)
  let path = "/var/run/docker.sock" in
  if Sys.file_exists path then
    if writable path then
      Log.app (fun f -> f "docker socket %s: ok (writable)" path)
    else
      Log.warn (fun f ->
          f
            "docker socket %s: present but not writable — check the daemon's \
             gid is in the docker group"
            path)
  else
    Log.warn (fun f ->
        f "docker socket %s: not present — base-image rebuild paths will fail"
          path)

(* OCurrent stores Op_build/Op_tool successes in sqlite with the
   layer_dir path as part of the marshalled value. If the cache
   directory was wiped (or moved between host and container, with
   different absolute paths), those entries point at directories
   that no longer exist — and OCurrent will happily return a cache
   hit without re-running, so the next mount fails with
   "special device overlay does not exist" once the runner tries
   to lowerdir= a missing path.

   Sample the first ~N successful build entries, count how many
   layer_dirs are present on disk. Anything under ~50% is a sign
   the two caches are out of sync. We don't depend on sqlite3-the-
   library here — we just shell out to the [sqlite3] binary if it's
   on PATH and skip the check otherwise. *)
let check_ocurrent_layer_alignment () =
  let db = "/var/lib/ocurrent/var/db/sqlite.db" in
  if not (Sys.file_exists db) then
    Log.app (fun f -> f "ocurrent db %s: not present (first run)" db)
  else if not (exec_present "sqlite3") then
    Log.app (fun f ->
        f
          "ocurrent db %s: present, but sqlite3 not on PATH — skipping \
           cross-check against on-disk layer dirs"
          db)
  else
    let cmd =
      Printf.sprintf
        "sqlite3 -readonly %s \"SELECT value FROM cache WHERE op IN \
         ('day11-build','day11-tool') AND ok=1 LIMIT 200\" 2>/dev/null"
        (Filename.quote db)
    in
    try
      let ic = Unix.open_process_in cmd in
      let lines = ref [] in
      (try
         while true do
           lines := input_line ic :: !lines
         done
       with End_of_file -> ());
      ignore (Unix.close_process_in ic);
      let extract_dir line =
        match String.index_opt line '"' with
        | None -> None
        | Some _ -> (
            (* Quick-and-dirty: pull the layer_dir field. The marshalled
             value is JSON like {"pkg":"...","hash":"...","layer_dir":"..."}.
             A real json parse here would drag yojson into this module;
             a substring match is enough for a sanity check. *)
            let needle = "\"layer_dir\":\"" in
            let nlen = String.length needle in
            match String.index_from_opt line 0 '\"' with
            | None -> None
            | Some _ ->
                let rec find i =
                  if i + nlen > String.length line then None
                  else if String.sub line i nlen = needle then
                    let start = i + nlen in
                    match String.index_from_opt line start '"' with
                    | None -> None
                    | Some endq -> Some (String.sub line start (endq - start))
                  else find (i + 1)
                in
                find 0)
      in
      let dirs = List.filter_map extract_dir !lines in
      let total = List.length dirs in
      if total = 0 then
        Log.app (fun f ->
            f
              "ocurrent layer-dir check: no day11-build/tool successes yet \
               (fresh cache)")
      else
        let missing = List.filter (fun d -> not (Sys.file_exists d)) dirs in
        let n_missing = List.length missing in
        let pct = 100 * n_missing / total in
        if n_missing = 0 then
          Log.app (fun f ->
              f
                "ocurrent layer-dir check: %d/%d sampled entries point at \
                 existing dirs"
                total total)
        else if pct < 25 then
          Log.warn (fun f ->
              f
                "ocurrent layer-dir check: %d of %d sampled entries point at \
                 missing layer dirs (%d%%) — some cache eviction expected, but \
                 watch for cascading rebuilds"
                n_missing total pct)
        else
          let example = match missing with d :: _ -> d | [] -> "" in
          Log.err (fun f ->
              f
                "ocurrent layer-dir check: %d of %d sampled entries point at \
                 MISSING layer dirs (%d%%). example: %s. The ocurrent db and \
                 the day11 cache are out of sync — builds will report \
                 cache-hits then fail to mount. Fix with: docker-compose down \
                 && docker volume rm <project>_ocurrent-data && docker-compose \
                 up -d"
                n_missing total pct example)
    with _ ->
      Log.warn (fun f -> f "ocurrent layer-dir check: failed to read %s" db)

let check_disk ~kind path =
  match disk_free_gib (Fpath.to_string path) with
  | None ->
      Log.warn (fun f ->
          f "%s %a: could not stat free space" kind Fpath.pp path)
  | Some g when g < 20 ->
      Log.err (fun f ->
          f "%s %a: only %d GiB free — layer cache will fill up fast" kind
            Fpath.pp path g)
  | Some g when g < 100 ->
      Log.warn (fun f ->
          f "%s %a: %d GiB free — keep an eye on it" kind Fpath.pp path g)
  | Some g -> Log.app (fun f -> f "%s %a: %d GiB free" kind Fpath.pp path g)

(* ── entry point ─────────────────────────────────────────────── *)

let run ~profile_dir ~cache_dir =
  Log.app (fun f -> f "── start-of-day diagnostics ──");
  check_identity ();
  check_path_writable ~kind:"profile-dir " profile_dir;
  check_path_writable ~kind:"cache-dir   " cache_dir;
  let tmpdir = try Some (Sys.getenv "TMPDIR") with Not_found -> None in
  (match tmpdir with
  | Some d -> check_path_writable ~kind:"TMPDIR      " (Fpath.v d)
  | None -> Log.app (fun f -> f "TMPDIR unset — using default /tmp"));
  check_disk ~kind:"cache fs " cache_dir;
  check_helper ~name:"runc" ~kind:"helper" ~version_args:[ "--version" ]
    ~severity:`Required;
  check_helper ~name:"git" ~kind:"helper" ~version_args:[ "--version" ]
    ~severity:`Required;
  check_helper ~name:"sudo" ~kind:"helper" ~version_args:[ "--version" ]
    ~severity:`Required;
  check_helper ~name:"docker" ~kind:"helper" ~version_args:[ "--version" ]
    ~severity:`Optional;
  check_helper ~name:"dot" ~kind:"helper" ~version_args:[ "-V" ]
    ~severity:`Optional;
  check_helper ~name:"rsync" ~kind:"helper" ~version_args:[ "--version" ]
    ~severity:`Optional;
  check_docker_socket ();
  check_ocurrent_layer_alignment ();
  Log.app (fun f -> f "── diagnostics complete ──")
