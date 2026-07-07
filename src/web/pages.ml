(** Dashboard page resources.

    Each page is a {!Current_web.Resource.t} that reads its data
    from the on-disk {!Day11_batch}/{!Day11_lib} layout — no caching,
    no background indexing — and renders via TyXML. The site
    chrome (top nav) is added by [Context.respond_ok].

    Wired up in {!Routes}; that module is the public entry point
    for ocaml-docs-ci to register these pages. *)

open Tyxml.Html
module Resource = Current_web.Resource
module Context = Current_web.Context
module Profile = Day11_batch.Profile

(** Shared per-process context. [profile_dir] is the directory that
    holds the [<name>.json] profile files; [cache_dir] is the day11
    cache root from which snapshot dirs are derived. *)
type ctx = {
  profile_dir : Fpath.t;
  cache_dir : Fpath.t;
}

(** Memoise an expensive file read by (path, mtime). The cache is
    process-wide and never trimmed — call sites are bounded (one
    [dag.json] per snapshot, one [layer_status.jsonl] per os_dir),
    and a stale entry is just memory, never wrong data, because the
    mtime changes the cache key. *)
let memo_by_mtime
    : type a. (Fpath.t, float * a) Hashtbl.t -> Fpath.t -> (unit -> a) -> a
    = fun cache p compute ->
  let p_s = Fpath.to_string p in
  let mtime = try (Unix.stat p_s).Unix.st_mtime with _ -> 0.0 in
  match Hashtbl.find_opt cache p with
  | Some (m, v) when m = mtime -> v
  | _ ->
    let v = compute () in
    Hashtbl.replace cache p (mtime, v);
    v

let dag_cache :
  (Fpath.t, float *
    (Day11_lib.Dag_marshal.entry list, [ `Msg of string ]) result)
  Hashtbl.t = Hashtbl.create 8

let read_dag_cached snapshot_dir =
  let p = Fpath.(snapshot_dir / "dag.json") in
  memo_by_mtime dag_cache p
    (fun () -> Day11_lib.Dag_marshal.read ~snapshot_dir)

let layer_status_cache :
  (Fpath.t, float * (string, Day11_layer.Layer_status.entry) Hashtbl.t)
  Hashtbl.t = Hashtbl.create 4

let load_layer_status_cached os_dir =
  let p = Fpath.(os_dir / "layer_status.jsonl") in
  memo_by_mtime layer_status_cache p
    (fun () -> Day11_layer.Layer_status.load ~os_dir)

(** [snapshots_base ctx name] is the on-disk dir holding all
    snapshots for the named profile. Mirrors
    [Day11_profile_ctx_loader.snapshots_base_for]. *)
let snapshots_base ctx name =
  Fpath.(parent ctx.cache_dir / "snapshots" / name)

(** Look up the OCurrent job_id for a given build_hash by querying the
    Current_cache sqlite db. Returns the most recent job_id (highest
    finished timestamp) or [None] if not cached. Accepts either the
    short (12-char) form or the full 32-char hash — we match on the
    [substr(key,1,N)] prefix for whichever length we got. *)
(* TODO: stop recovering the job id from OCurrent's cache db. We already
   hold [Current.Job.id job] at build time in [day11_prep.ml]'s [Op.build]
   (line ~95) and throw it away. Better: stash [hash -> job_id] there and
   persist it on the [History.entry] the recorder appends, so the web
   reads [e.job_id] straight from history.jsonl. That is durable,
   per-entry (each retry/older run links to *its own* job, not just the
   latest), and drops the coupling to OCurrent's cache schema + db path.
   Keep the sqlite lookup below as a fallback for pre-existing snapshots.
   Plumbing: hash->job_id map populated before [ctx.dispatch]
   (day11_prep.ml:134), new [job_id] field on History.entry, prefer it in
   [job_id_for_hash]/the package page.

   OCurrent keeps its cache db at <state-dir>/var/db/sqlite.db. Resolve
   it at query time: [DOCS_CI_JOB_DB] wins if set, otherwise pick the
   first candidate that exists — the container's image WORKDIR
   ([/var/lib/ocurrent]) then the CWD-relative OCurrent default. Avoids
   baking in any one deployment's absolute path. *)
let job_db_path () =
  match Sys.getenv_opt "DOCS_CI_JOB_DB" with
  | Some p -> p
  | None ->
    let candidates =
      [ "/var/lib/ocurrent/var/db/sqlite.db"; "var/db/sqlite.db" ] in
    (try List.find Sys.file_exists candidates
     with Not_found -> "var/db/sqlite.db")

let job_id_for_hash hash =
  let n = min 12 (String.length hash) in
  let prefix = String.sub hash 0 n in
  let job_db_path = job_db_path () in
  if not (Sys.file_exists job_db_path) then None
  else
    try
      let db = Sqlite3.db_open ~mode:`READONLY job_db_path in
      let stmt = Sqlite3.prepare db
        "SELECT job_id FROM cache \
         WHERE substr(key,1,?) = ? AND op LIKE 'day11-%' \
         ORDER BY finished DESC LIMIT 1" in
      let _ = Sqlite3.bind_int stmt 1 n in
      let _ = Sqlite3.bind_blob stmt 2 prefix in
      let result = ref None in
      (match Sqlite3.step stmt with
       | Sqlite3.Rc.ROW ->
         (match Sqlite3.column stmt 0 with
          | Sqlite3.Data.TEXT s -> result := Some s
          | _ -> ())
       | _ -> ());
      ignore (Sqlite3.finalize stmt);
      ignore (Sqlite3.db_close db);
      !result
    with _ -> None

(** Batched variant. Issues a single SELECT for the union of given
    hashes (12-char prefixes) instead of one query per hash. The
    [snapshot_detail] failures table calls this once with all the
    failed-node hashes — 100+ SQL round-trips collapse to one. *)
let job_ids_for_hashes hashes =
  let result : (string, string) Hashtbl.t = Hashtbl.create 64 in
  let prefixes = List.filter_map (fun h ->
    if String.length h = 0 then None
    else Some (String.sub h 0 (min 12 (String.length h)))) hashes in
  let job_db_path = job_db_path () in
  match prefixes with
  | [] -> result
  | _ when not (Sys.file_exists job_db_path) -> result
  | _ ->
    try
      let db = Sqlite3.db_open ~mode:`READONLY job_db_path in
      let placeholders = String.concat ","
        (List.mapi (fun i _ -> Printf.sprintf "?%d" (i + 1)) prefixes) in
      let sql = Printf.sprintf
        "SELECT substr(key,1,12) AS prefix, job_id, MAX(finished) \
         FROM cache \
         WHERE substr(key,1,12) IN (%s) AND op LIKE 'day11-%%' \
         GROUP BY substr(key,1,12)"
        placeholders in
      let stmt = Sqlite3.prepare db sql in
      List.iteri (fun i p ->
        ignore (Sqlite3.bind_blob stmt (i + 1) p)) prefixes;
      let rec loop () =
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
          (match Sqlite3.column stmt 0, Sqlite3.column stmt 1 with
           | Sqlite3.Data.BLOB p, Sqlite3.Data.TEXT j
           | Sqlite3.Data.TEXT p, Sqlite3.Data.TEXT j ->
             Hashtbl.replace result p j
           | _ -> ());
          loop ()
        | _ -> ()
      in
      loop ();
      ignore (Sqlite3.finalize stmt);
      ignore (Sqlite3.db_close db);
      result
    with _ -> result

(** The snapshot's own creation time: the ISO-8601 UTC timestamp from
    [repos.json] (written by [Snapshot.save]). Falls back to the
    directory mtime, formatted identically, only when repos.json is
    missing/unreadable. We deliberately do NOT use the directory mtime
    as the primary signal: it is bumped whenever any file inside is
    rewritten (e.g. the [pkgs_summary] regeneration on startup), which
    can float a long-finished snapshot above the live one — the cause of
    the dashboard featuring a stale "finished" snapshot as latest. *)
let snapshot_created dir =
  match Day11_batch.Snapshot.load dir with
  | Ok s -> s.Day11_batch.Snapshot.created
  | Error _ ->
    (try
       let tm =
         Unix.gmtime (Unix.stat (Fpath.to_string dir)).Unix.st_mtime in
       Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
         (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
         tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
     with _ -> "")

(** List a profile's snapshots, newest first by creation time. ISO-8601
    UTC timestamps sort lexicographically in chronological order, so a
    plain string compare orders them correctly. *)
let list_snapshots_newest_first ctx name =
  let base = snapshots_base ctx name in
  match Bos.OS.Dir.contents base with
  | Error _ -> []
  | Ok entries ->
    entries
    |> List.filter_map (fun p ->
      if Bos.OS.Dir.exists p |> Result.value ~default:false
      then Some (p, snapshot_created p) else None)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map fst

(** Read [packages/] under a snapshot dir. *)
let snapshot_packages snapshot_dir =
  let pdir = Fpath.(snapshot_dir / "packages") in
  match Bos.OS.Dir.contents pdir with
  | Error _ -> []
  | Ok entries -> List.map Fpath.basename entries |> List.sort compare

(** Latest status from [packages/<pkg>/history.jsonl] in a snapshot
    dir, or [None] if the file is missing or empty. Reads the LAST
    line of the file (the rolling history is append-only). *)
let latest_pkg_status snapshot_dir pkg_str =
  let h = Fpath.(snapshot_dir / "packages" / pkg_str / "history.jsonl") in
  match Bos.OS.File.read_lines h with
  | Error _ -> None
  | Ok lines ->
    let last = List.fold_left (fun _ l -> l) "" lines in
    if last = "" then None
    else
      try
        let json = Yojson.Safe.from_string last in
        let open Yojson.Safe.Util in
        Some (json |> member "status" |> to_string)
      with _ -> None

(** Latest [(status, category, build_hash)] for a package, or [None]. *)
let latest_pkg_status_full snapshot_dir pkg_str =
  let h = Fpath.(snapshot_dir / "packages" / pkg_str / "history.jsonl") in
  match Bos.OS.File.read_lines h with
  | Error _ -> None
  | Ok lines ->
    let last = List.fold_left (fun _ l -> l) "" lines in
    if last = "" then None
    else
      try
        let json = Yojson.Safe.from_string last in
        let open Yojson.Safe.Util in
        let status = json |> member "status" |> to_string in
        let category =
          try json |> member "category" |> to_string
          with _ -> status in
        let build_hash =
          try json |> member "build_hash" |> to_string
          with _ -> "" in
        Some (status, category, build_hash)
      with _ -> None

(** Find the snapshot key chronologically just before [current_key]
    in this profile, by mtime. Returns [None] if [current_key] is the
    oldest. Used for the "Diff against previous" button on
    {!snapshot_detail}. *)
let find_previous_snapshot_key ctx name current_key =
  let snaps = list_snapshots_newest_first ctx name in
  let keys = List.map Fpath.basename snaps in
  let rec walk = function
    | [] | [_] -> None
    | k :: next :: _ when k = current_key -> Some next
    | _ :: rest -> walk rest
  in
  walk keys

(* ── /profiles ────────────────────────────────────────────────── *)

let profiles_index ~ctx =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! nav_link = Some "Profiles"
    method! private get web_ctx =
      let names = Profile.list ~dir:ctx.profile_dir in
      let row name =
        let snaps = list_snapshots_newest_first ctx name in
        let snap_count = List.length snaps in
        let latest = match snaps with
          | [] -> txt "—"
          | s :: _ ->
            let key = Fpath.basename s in
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                              name key) ]
              [ Templates.sha_span key ]
        in
        tr [
          td [ a ~a:[ a_href ("/profiles/" ^ name) ] [ txt name ] ];
          td [ txt (string_of_int snap_count) ];
          td [ latest ];
        ]
      in
      Context.respond_ok web_ctx [
        Templates.style_block;
        h2 [ txt "Profiles" ];
        table ~a:[ a_class [ "data" ] ]
          ~thead:(thead [ tr [ th [ txt "Name" ];
                               th [ txt "Snapshots" ];
                               th [ txt "Latest" ] ] ])
          (List.map row names)
      ]
  end

(* ── /profiles/<name> ─────────────────────────────────────────── *)

let profile_dashboard ~ctx name =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let snaps = list_snapshots_newest_first ctx name in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles"; None, name
      ] in
      let body = match snaps with
        | [] -> [ p [ txt "No snapshots yet for this profile." ] ]
        | s :: _ ->
          let key = Fpath.basename s in
          let snapshot_link =
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                              name key) ]
              [ txt key ] in
          let snapshots_link =
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots" name) ]
              [ txt (Printf.sprintf "All snapshots (%d)"
                       (List.length snaps)) ] in
          let recent_link =
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/recent" name) ]
              [ txt "Recent changes" ] in
          [ p [ txt "Latest snapshot: "; snapshot_link ];
            ul [ li [ recent_link ];
                 li [ snapshots_link ] ] ]
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs; h2 [ txt name ] ] @ body)
  end

(* ── /profiles/<name>/snapshots[?page=N] ──────────────────────── *)

let page_size = 25

let snapshots_list ~ctx name =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let req = Context.request web_ctx in
      let uri = Cohttp.Request.uri req in
      let page =
        match Uri.get_query_param uri "page" with
        | Some s -> (try max 1 (int_of_string s) with _ -> 1)
        | None -> 1
      in
      let snaps = list_snapshots_newest_first ctx name in
      let total = List.length snaps in
      let n_pages = max 1 ((total + page_size - 1) / page_size) in
      let page = min page n_pages in
      let start = (page - 1) * page_size in
      let visible = snaps
        |> List.filteri (fun i _ -> i >= start && i < start + page_size) in
      let row dir =
        let key = Fpath.basename dir in
        let created = match snapshot_created dir with "" -> "—" | s -> s in
        tr [
          td [ a ~a:[ a_href (Printf.sprintf "/profiles/%s/snapshots/%s"
                                 name key) ]
                 [ Templates.sha_span key ] ];
          td [ txt created ];
        ]
      in
      let pager =
        if n_pages <= 1 then []
        else
          let link p label =
            a ~a:[ a_href (Printf.sprintf
                             "/profiles/%s/snapshots?page=%d" name p) ]
              [ txt label ]
          in
          [ div ~a:[ a_class [ "pager" ] ]
              (List.concat [
                (if page > 1 then [ link (page - 1) "‹ Prev"; txt " " ]
                 else []);
                [ txt (Printf.sprintf "Page %d of %d (%d snapshots)"
                         page n_pages total) ];
                (if page < n_pages then [ txt " "; link (page + 1) "Next ›" ]
                 else []);
              ]) ]
      in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "Snapshots";
      ] in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt (name ^ " — snapshots") ];
           table ~a:[ a_class [ "data" ] ]
             ~thead:(thead [ tr [ th [ txt "Key" ];
                                  th [ txt "Created (UTC)" ] ] ])
             (List.map row visible) ]
         @ pager)
  end

(* ── /profiles/<name>/snapshots/<key> ─────────────────────────── *)

let snapshot_detail ~ctx name key =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let _t0 = Unix.gettimeofday () in
      let timing label fn =
        let s = Unix.gettimeofday () in
        let r = fn () in
        Printf.eprintf "[snapshot_detail %s] %s: %.3fs\n%!" key label
          (Unix.gettimeofday () -. s);
        r
      in
      let snapshot_dir = Fpath.(snapshots_base ctx name / key) in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/snapshots" name), "Snapshots";
        None, key;
      ] in
      let repos_json =
        match Bos.OS.File.read Fpath.(snapshot_dir / "repos.json") with
        | Error _ -> None
        | Ok s -> (try Some (Yojson.Safe.from_string s) with _ -> None)
      in
      let repos = match repos_json with
        | None -> []
        | Some json ->
          (try
             let open Yojson.Safe.Util in
             json |> member "repos" |> to_list
             |> List.map (fun r ->
               let path = r |> member "path" |> to_string in
               let commit = r |> member "commit" |> to_string in
               (path, commit))
           with _ -> [])
      in
      (* The ISO-8601 timestamp of when this snapshot was first seen,
         written by [Snapshot.save]. Surfaced in the page header. *)
      let created = match repos_json with
        | None -> None
        | Some json ->
          (match Yojson.Safe.Util.member "created" json with
           | `String s -> Some s
           | _ -> None
           | exception _ -> None)
      in
      (* Read the live HEAD of [path] (a local git repo). Returns
         [None] if [path] isn't a git repo or the read fails — non-git
         paths (rare for tracked repos) just leave the "Latest" cell
         blank. Captures stderr alongside stdout so a transient
         "fatal: ambiguous argument" doesn't pollute the page. *)
      let read_live_head path =
        let cmd = Printf.sprintf
          "git -C %s rev-parse HEAD 2>/dev/null"
          (Filename.quote path) in
        try
          let ic = Unix.open_process_in cmd in
          let line = try Some (String.trim (input_line ic))
                     with End_of_file -> None in
          let _ = Unix.close_process_in ic in
          match line with
          | Some s when String.length s = 40 -> Some s
          | _ -> None
        with _ -> None
      in
      (* Subject line of [commit] in the git repo at [path]. Returns
         [None] if the commit isn't present locally or the read fails. *)
      let read_commit_subject path commit =
        let cmd = Printf.sprintf
          "git -C %s log -1 --format=%%s %s 2>/dev/null"
          (Filename.quote path) (Filename.quote commit) in
        try
          let ic = Unix.open_process_in cmd in
          let line = try Some (String.trim (input_line ic))
                     with End_of_file -> None in
          let _ = Unix.close_process_in ic in
          (match line with Some "" -> None | l -> l)
        with _ -> None
      in
      (* For a github-pin-overlay path of the form ".../overlays/<n>/repo",
         the [.../overlays/<n>/upstream] sibling holds the actual
         upstream clone (e.g. ocaml/odoc or jonludlam/odoc). Surface
         its current HEAD too so the user sees the underlying sha
         being tracked, not just the overlay-repo's bookkeeping sha. *)
      let upstream_head_for path =
        let suffix = "/repo" in
        if Astring.String.is_suffix ~affix:suffix path then
          let base = String.sub path 0 (String.length path - String.length suffix) in
          let upstream = base ^ "/upstream" in
          if Sys.file_exists upstream
          then Option.map (fun h -> (upstream, h)) (read_live_head upstream)
          else None
        else None
      in
      let repos_table = match repos with
        | [] -> p [ em [ txt "No repos.json on disk." ] ]
        | _ ->
          let row (p, c) =
            let live = read_live_head p in
            let live_cell = match live with
              | None -> em [ txt "—" ]
              | Some h when h = c -> Templates.sha_span h
              | Some h -> span ~a:[ a_class [ "warn" ] ]
                  [ Templates.sha_span h ]
            in
            let msg_cell = match read_commit_subject p c with
              | Some m -> td [ txt m ]
              | None -> td [ em [ txt "—" ] ]
            in
            let upstream_rows = match upstream_head_for p with
              | None -> []
              | Some (upath, uhead) ->
                [ tr [ td [ code [ txt (upath ^ " (upstream)") ] ];
                       td [ em [ txt "—" ] ];
                       td [ em [ txt "—" ] ];
                       td [ Templates.sha_span uhead ] ] ]
            in
            tr [ td [ code [ txt p ] ];
                 td [ Templates.sha_span c ];
                 msg_cell;
                 td [ live_cell ] ] :: upstream_rows
          in
          table ~a:[ a_class [ "data" ] ]
            ~thead:(thead [ tr [ th [ txt "Repo" ];
                                 th [ txt "Snapshot" ];
                                 th [ txt "Message" ];
                                 th [ txt "Latest" ] ] ])
            (List.concat_map row repos)
      in
      let totals =
        match Day11_lib.Status_index.read ~dir:snapshot_dir with
        | None -> [ p [ em [ txt "Status not yet generated for \
                                  this snapshot — run is in \
                                  progress or pre-finish." ] ] ]
        | Some st ->
          let is_doc_cat c =
            c = "doc_success" || c = "doc_failure"
            || c = "doc_dependency_failure"
          in
          let partition rows =
            List.partition (fun (c, _) -> is_doc_cat c) rows
          in
          let build_blessed = snd (partition st.blessed_totals) in
          let doc_blessed = fst (partition st.blessed_totals) in
          let build_nonblessed = snd (partition st.non_blessed_totals) in
          let doc_nonblessed = fst (partition st.non_blessed_totals) in
          let breakdown_row label rows =
            let parts = List.map
              (fun (cat, n) -> Printf.sprintf "%s=%d" cat n) rows in
            let total = List.fold_left (fun acc (_, n) -> acc + n) 0 rows in
            let txt_str = match parts with
              | [] -> "0"
              | _ -> Printf.sprintf "%d (%s)" total
                       (String.concat ", " parts) in
            tr [ th [ txt label ]; td [ txt txt_str ] ]
          in
          [ table ~a:[ a_class [ "data" ] ]
              [ breakdown_row "Blessed builds" build_blessed;
                breakdown_row "Non-blessed builds" build_nonblessed;
                breakdown_row "Blessed docs" doc_blessed;
                breakdown_row "Non-blessed docs" doc_nonblessed ];
            p ~a:[ a_class [ "crumbs" ] ]
              [ em [ txt "Builds count compiled package layers; docs \
                          count compile+link (or doc-all) stages. Counts \
                          are per node (a package solved in N universes \
                          counts as N). 'Blessed' means the node belongs \
                          to the chosen primary universe for its package; \
                          build nodes aren't universe-specific, so blessed \
                          builds is normally empty. A dependency_failure \
                          is a cascade — the node never ran because a \
                          dependency failed." ] ] ]
      in
      let pkgs = snapshot_packages snapshot_dir in
      let pkg_link p =
        match String.index_opt p '.' with
        | None ->
          a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s" name p) ]
            [ txt p ]
        | Some i ->
          let n = String.sub p 0 i in
          let v = String.sub p (i + 1) (String.length p - i - 1) in
          a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s/%s" name n v) ]
            [ txt p ]
      in
      (* pkg_table is rendered after [dag_data] is available so we
         can list every package from dag.json (not just those with
         per-snapshot history). See further down. *)
      let _ = pkgs in
      let diff_link = match find_previous_snapshot_key ctx name key with
        | None -> []
        | Some prev ->
          [ p [ a ~a:[ a_href (Printf.sprintf
                                 "/profiles/%s/snapshots/%s/diff/%s"
                                 name prev key) ]
                  [ txt "Diff against previous snapshot ("
                  ; Templates.sha_span prev
                  ; txt ")" ] ] ]
      in
      let kind_label : Day11_lib.Dag_marshal.kind -> string = function
        | Build -> "build" | Tool -> "tool" | Compile -> "compile"
        | Doc_all -> "doc-all" | Link -> "link"
      in
      (* Read dag.json + classify once. Drives Failures list, DAG
         overview, and cascade breakdown — all want the same view. *)
      let dag_data = timing "dag_data" (fun () ->
        match timing "read_dag_cached" (fun () ->
          read_dag_cached snapshot_dir) with
        | Error _ -> None
        | Ok entries ->
          let os_dir =
            match Profile.load ~dir:ctx.profile_dir ~name with
            | Ok profile ->
              Some Fpath.(ctx.cache_dir / Profile.os_dir_name profile)
            | Error _ -> None
          in
          let cascade_table = match os_dir with
            | Some d ->
              let status_index = timing "load_layer_status_cached"
                (fun () -> load_layer_status_cached d) in
              timing "classify_from_layer_index" (fun () ->
                Day11_lib.Cascade.classify_from_layer_index
                  ~status_index entries)
            | None ->
              let packages_dir = Fpath.(snapshot_dir / "packages") in
              Day11_lib.Cascade.classify ~packages_dir entries
          in
          Some (entries, cascade_table))
      in
      (* Surface failed nodes prominently. Driven by cascade_table when
         dag.json is present (comprehensive: includes failures cached
         from previous snapshots that didn't re-dispatch); falls back
         to per-snapshot history for older snapshots without dag.json. *)
      let failures_section = timing "failures_section" (fun () ->
        match dag_data with
        | Some (entries, cascade_table) ->
          let failed = List.filter_map (fun (e : Day11_lib.Dag_marshal.entry) ->
            match Hashtbl.find_opt cascade_table e.hash with
            | Some { Day11_lib.Cascade.status = Failed; _ } -> Some e
            | _ -> None) entries
          in
          (match failed with
           | [] -> [ p [ em [ txt "No failed nodes 🎉" ] ] ]
           | _ ->
             let by_kind = List.fold_left (fun acc (e : Day11_lib.Dag_marshal.entry) ->
               let k = kind_label e.kind in
               let n = try List.assoc k acc with Not_found -> 0 in
               (k, n + 1) :: List.filter (fun (kk, _) -> kk <> k) acc) [] failed in
             let by_kind = List.sort (fun (_, a) (_, b) -> compare b a) by_kind in
             let summary = String.concat ", " (List.map
               (fun (k, n) -> Printf.sprintf "%s=%d" k n) by_kind) in
             let sorted = List.sort (fun (a : Day11_lib.Dag_marshal.entry) b ->
               compare (OpamPackage.to_string a.pkg)
                       (OpamPackage.to_string b.pkg)) failed in
             (* Batch SQLite lookup: one query for all failed hashes
                instead of one per row (was 50+ ms × 100s of rows = the
                whole page). *)
             let job_ids = job_ids_for_hashes
               (List.map (fun (e : Day11_lib.Dag_marshal.entry) ->
                  e.hash) sorted) in
             let row (e : Day11_lib.Dag_marshal.entry) =
               let pkg_cell = pkg_link (OpamPackage.to_string e.pkg) in
               let prefix = String.sub e.hash 0
                 (min 12 (String.length e.hash)) in
               let log_target =
                 match Hashtbl.find_opt job_ids prefix with
                 | Some job_id -> "/job/" ^ job_id
                 | None ->
                   Printf.sprintf "/profiles/%s/builds/%s/log" name e.hash
               in
               tr [ td [ pkg_cell ];
                    td [ txt (kind_label e.kind) ];
                    td [ Templates.sha_span e.hash ];
                    td [ a ~a:[ a_href log_target ] [ txt "log" ] ] ]
             in
             [ p [ txt (Printf.sprintf "%d failed (%s)"
                          (List.length failed) summary) ];
               table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Kind" ];
                                      th [ txt "Hash" ];
                                      th [ txt "Log" ] ] ])
                 (List.map row sorted) ])
        | None ->
          (* Fallback: legacy per-snapshot history view. *)
          let failures, with_history =
            List.fold_left (fun (fs, wh) pkg_str ->
              match latest_pkg_status_full snapshot_dir pkg_str with
              | Some (status, _, _) when status = "success" -> (fs, wh + 1)
              | Some (status, category, build_hash) ->
                ((pkg_str, status, category, build_hash) :: fs, wh + 1)
              | None -> (fs, wh)) ([], 0) pkgs
          in
          let failures = List.rev failures in
          let total_pkgs = List.length pkgs in
          let no_history = total_pkgs - with_history in
          (match failures with
           | [] when total_pkgs = 0 ->
             [ p [ em [ txt "Snapshot still being prepared — no dag.json \
                              written yet, and no per-package history \
                              recorded. Check back once the run reaches \
                              dispatch." ] ] ]
           | [] when no_history > 0 ->
             [ p [ em [ txt (Printf.sprintf
               "No dag.json and no history yet (%d/%d packages)."
               no_history total_pkgs) ] ] ]
           | [] -> [ p [ em [ txt "No failed packages 🎉" ] ] ]
           | _ ->
             let row (pkg_str, status, category, _build_hash) =
               tr [ td [ pkg_link pkg_str ];
                    td [ Templates.status_span status ];
                    td [ txt category ] ]
             in
             [ table ~a:[ a_class [ "data" ] ]
                 ~thead:(thead [ tr [ th [ txt "Package" ];
                                      th [ txt "Status" ];
                                      th [ txt "Category" ] ] ])
                 (List.map row failures) ]))
      in
      (* Comprehensive DAG overview from on-disk layer state, plus a
         per-cascade breakdown when there's something to show. Both
         derived from one [Cascade.classify_from_layers] pass over
         dag.json + [<os_dir>/layer_status.jsonl]. *)
      let overview_section, cascade_section = timing "overview+cascade" (fun () ->
        match dag_data with
        | None -> [], []
        | Some (entries, cascade_table) ->
          let bucket_for : Day11_lib.Cascade.status -> string = function
            | Ok -> "ok" | Failed -> "failed"
            | Cascade _ -> "cascade" | Pending -> "pending"
          in
          let counts : (string * string, int) Hashtbl.t =
            Hashtbl.create 32 in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            match Hashtbl.find_opt cascade_table e.hash with
            | None -> ()
            | Some r ->
              let k = (kind_label e.kind, bucket_for r.status) in
              let n = try Hashtbl.find counts k with Not_found -> 0 in
              Hashtbl.replace counts k (n + 1)
          ) entries;
          let kinds = ["build"; "tool"; "compile"; "doc-all"; "link"] in
          let buckets = ["ok"; "failed"; "cascade"; "pending"] in
          let overview_rows = List.map (fun k ->
            let cells = List.map (fun b ->
              let n = try Hashtbl.find counts (k, b) with Not_found -> 0 in
              td [ txt (string_of_int n) ]) buckets in
            tr (th [ txt k ] :: cells)) kinds
          in
          let total_for b = List.fold_left (fun acc k ->
            acc + (try Hashtbl.find counts (k, b)
                   with Not_found -> 0)) 0 kinds in
          let pending_total = total_for "pending" in
          let overview =
            if pending_total > 0 then
              (* Run still in flight — show the live per-kind table so
                 progress (ok vs pending) is visible at a glance. *)
              [ h3 [ txt "DAG state" ];
                p [ em [ txt "Every planned node classified by on-disk \
                              layer status. Cascade = dispatch skipped \
                              because a dep failed." ] ];
                table ~a:[ a_class [ "data" ] ]
                  ~thead:(thead [ tr (th [ txt "Kind" ] ::
                    List.map (fun b -> th [ txt b ]) buckets) ])
                  overview_rows ]
            else
              (* Nothing pending — the run has finished; collapse the
                 table to a one-line summary. *)
              let ok = total_for "ok" and failed = total_for "failed"
              and cascade = total_for "cascade" in
              [ h3 [ txt "DAG state" ];
                p [ txt (Printf.sprintf
                  "All %d nodes finished — ok=%d, failed=%d, cascade=%d"
                  (ok + failed + cascade) ok failed cascade) ] ]
          in
          let by_hash : (string, Day11_lib.Dag_marshal.entry) Hashtbl.t =
            Hashtbl.create (List.length entries) in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            Hashtbl.replace by_hash e.hash e) entries;
          let cascaded =
            List.filter_map (fun (e : Day11_lib.Dag_marshal.entry) ->
              match Hashtbl.find_opt cascade_table e.hash with
              | Some { Day11_lib.Cascade.status = Cascade src; _ } ->
                Some (e, src)
              | _ -> None
            ) entries
          in
          let cascade =
            if cascaded = [] then []
            else begin
              let row ((e : Day11_lib.Dag_marshal.entry), src) =
                let src_e = Hashtbl.find by_hash src in
                tr [ td [ pkg_link (OpamPackage.to_string e.pkg) ];
                     td [ txt (kind_label e.kind) ];
                     td [ pkg_link (OpamPackage.to_string src_e.pkg) ];
                     td [ txt (kind_label src_e.kind) ] ]
              in
              let by_root = Hashtbl.create 16 in
              List.iter (fun (e, src) ->
                let prev = try Hashtbl.find by_root src
                  with Not_found -> [] in
                Hashtbl.replace by_root src ((e, src) :: prev)
              ) cascaded;
              let groups = Hashtbl.fold (fun root rs acc ->
                (root, rs) :: acc) by_root [] in
              let groups = List.sort (fun (_, a) (_, b) ->
                compare (List.length b) (List.length a)) groups in
              let rows = List.concat_map (fun (_, rs) ->
                let sorted = List.sort
                  (fun ((a : Day11_lib.Dag_marshal.entry), _)
                       ((b : Day11_lib.Dag_marshal.entry), _) ->
                    compare (OpamPackage.to_string a.pkg)
                            (OpamPackage.to_string b.pkg))
                  rs in
                List.map row sorted) groups
              in
              [ h3 [ txt (Printf.sprintf "Cascaded (%d)"
                            (List.length cascaded)) ];
                p [ em [ txt "Nodes that didn't run because an upstream \
                              node failed." ] ];
                table ~a:[ a_class [ "data" ] ]
                  ~thead:(thead [ tr [ th [ txt "Package" ];
                                       th [ txt "Kind" ];
                                       th [ txt "Blocked by" ];
                                       th [ txt "Kind" ] ] ])
                  rows ]
            end
          in
          overview, cascade)
      in
      (* Build pkg_table from dag.json (every planned package, sorted)
         with status from cascade_table. Falls back to the legacy
         per-snapshot list when dag.json is missing. *)
      let pkg_count, pkg_table = timing "pkg_table" (fun () ->
        match dag_data with
        | Some (entries, cascade_table) ->
          (* Per-package, two separate columns: Build status (build/tool
             kinds) and Doc status (compile/doc_all/link kinds). Each
             aggregated worst-of across that package's entries of the
             relevant kinds. Reflects what users care about: "did the
             package build" and "did its docs build" are independent
             concerns; merging them as one column hides doc failures
             behind a successful build. *)
          let by_name :
            (string,
              Day11_lib.Cascade.status list
              * Day11_lib.Cascade.status list) Hashtbl.t =
            Hashtbl.create 4096 in
          let kind_bucket : Day11_lib.Dag_marshal.kind -> [`Build | `Doc | `Skip] =
            function
            | Build | Tool -> `Build
            | Compile | Doc_all | Link -> `Doc
          in
          List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
            match kind_bucket e.kind with
            | `Skip -> ()
            | bucket ->
              let name = OpamPackage.to_string e.pkg in
              let st = match Hashtbl.find_opt cascade_table e.hash with
                | Some r -> r.status
                | None -> Day11_lib.Cascade.Pending
              in
              let bs, ds = try Hashtbl.find by_name name
                with Not_found -> ([], []) in
              let bs', ds' = match bucket with
                | `Build -> (st :: bs, ds)
                | `Doc -> (bs, st :: ds)
                | `Skip -> (bs, ds)
              in
              Hashtbl.replace by_name name (bs', ds')
          ) entries;
          let aggregate sts =
            (* Worst-of (Failed > Cascade > Pending > Ok) — surfaces
               problems on packages with multiple universes. Empty list
               means this column doesn't apply (e.g. tool-only package
               with no doc kinds), shown as "—". *)
            if sts = [] then None
            else if List.exists (fun s -> s = Day11_lib.Cascade.Failed) sts
            then Some "failed"
            else if List.exists (function
                | Day11_lib.Cascade.Cascade _ -> true | _ -> false) sts
            then Some "cascade"
            else if List.exists (fun s -> s = Day11_lib.Cascade.Pending) sts
            then Some "pending"
            else Some "ok"
          in
          let cell sts = match aggregate sts with
            | Some s -> td [ Templates.status_span s ]
            | None -> td [ em [ txt "—" ] ]
          in
          let names = Hashtbl.fold (fun n _ acc -> n :: acc) by_name [] in
          let names = List.sort compare names in
          let row name =
            let bs, ds = Hashtbl.find by_name name in
            tr [ td [ pkg_link name ]; cell bs; cell ds ]
          in
          (List.length names,
           table ~a:[ a_class [ "data" ] ]
             ~thead:(thead [ tr [ th [ txt "Package" ];
                                  th [ txt "Build" ];
                                  th [ txt "Doc" ] ] ])
             (List.map row names))
        | None ->
          let pkg_row p =
            let status_cell = match latest_pkg_status snapshot_dir p with
              | Some s -> Templates.status_span s
              | None -> em [ txt "—" ]
            in
            tr [ td [ pkg_link p ]; td [ status_cell ] ]
          in
          (List.length pkgs,
           match pkgs with
           | [] -> p [ em [ txt "No packages tracked yet." ] ]
           | _ ->
             table ~a:[ a_class [ "data" ] ]
               ~thead:(thead [ tr [ th [ txt "Package" ];
                                    th [ txt "Status" ] ] ])
               (List.map pkg_row pkgs)))
      in
      let created_line = match created with
        | None -> []
        | Some ts ->
          [ p ~a:[ a_class [ "crumbs" ] ] [ em [ txt ("Created " ^ ts) ] ] ]
      in
      (* Only the per-cascade breakdown (which node was blocked by which)
         is verbose and secondary; tuck that behind a fold. The DAG-state
         overview stays visible at the top as the live progress gauge. *)
      let cascade_fold = match cascade_section with
        | [] -> []
        | content ->
          [ details (summary [ txt "Cascaded nodes" ]) content ]
      in
      let r = timing "respond_ok+render" (fun () ->
        Context.respond_ok web_ctx ([
          Templates.style_block; crumbs;
          h2 [ txt (name ^ " / "); Templates.sha_span key ];
        ] @ created_line
          @ diff_link
          @ overview_section
          @ [ h3 [ txt "Repos at this snapshot" ]; repos_table ]
          @ [ h3 [ txt "Failures" ] ]
          @ failures_section
          @ cascade_fold
          @ [
          h3 [ txt "Status totals" ];
        ] @ totals @ [
          h3 [ txt (Printf.sprintf "Packages (%d)" pkg_count) ];
          pkg_table;
        ]))
      in
      Printf.eprintf "[snapshot_detail %s] TOTAL: %.3fs\n%!"
        key (Unix.gettimeofday () -. _t0);
      r
  end

(* ── Diff helpers ─────────────────────────────────────────────── *)

(* A package's per-snapshot identity is [(name, version)] so that
   two versions of the same package (e.g. [astring.0.8.3] AND
   [astring.0.8.5] both present in the newer snapshot) each get
   their own row instead of collapsing together. *)

type pkg_change =
  | Added of string * string * string
    (** [(version, status, build_hash)] — fresh package in the new
        snapshot. [build_hash] is the dispatched build's hash so the
        status cell can deep-link to logs / docs; empty string when
        unknown. *)
  | Removed of string
    (** [version] — package present in old, gone in new. *)
  | Status_changed of string * string * string * string
    (** [(version, old_status, new_status, new_build_hash)] — same
        (name, version), different status. *)
  | Version_changed of string * string * string * string
    (** [(old_version, new_version, new_status, new_build_hash)] —
        single old version replaced by a single new version
        (collapsed Removed+Added). *)

let split_pkg pkg_str =
  match String.index_opt pkg_str '.' with
  | None -> None
  | Some i ->
    let n = String.sub pkg_str 0 i in
    let v = String.sub pkg_str (i + 1) (String.length pkg_str - i - 1) in
    Some (n, v)

(* Aggregate a list of [(status, build_hash)] entries (one per
   universe of the same (name, version)) into a single
   [(status_str, hash)] pair. Status is the highest-priority status
   present (failure > cascade > pending > success); the chosen hash
   is the first entry with the chosen status, so failed builds yield
   a hash that links to a failed build log. Returns empty string
   for the hash when [entries] is empty. *)
let aggregate_pkg_status_with_hash entries =
  let is_failed = function Day11_lib.Cascade.Failed -> true | _ -> false in
  let is_cascade = function
    | Day11_lib.Cascade.Cascade _ -> true | _ -> false in
  let is_pending = function
    | Day11_lib.Cascade.Pending -> true | _ -> false in
  let pick filter =
    match List.find_opt (fun (s, _) -> filter s) entries with
    | Some (_, h) -> h
    | None -> ""
  in
  if List.exists (fun (s, _) -> is_failed s) entries
  then ("failure", pick is_failed)
  else if List.exists (fun (s, _) -> is_cascade s) entries
  then ("cascade", pick is_cascade)
  else if List.exists (fun (s, _) -> is_pending s) entries
  then ("pending", pick is_pending)
  else
    let h = match entries with (_, h) :: _ -> h | [] -> "" in
    ("success", h)

let os_dir_for ~ctx name =
  match Profile.load ~dir:ctx.profile_dir ~name with
  | Ok profile -> Some Fpath.(ctx.cache_dir / Profile.os_dir_name profile)
  | Error _ -> None

(* The profile's [html_dir], if configured. Used to check whether
   rendered HTML actually exists for a (pkg, ver) before linking to
   it — voodoo can fail for individual packages even when the build
   layer succeeded, leaving the docs link 404-ing. *)
let html_dir_for ~ctx name =
  match Profile.load ~dir:ctx.profile_dir ~name with
  (* Read through the [html-live] symlink — the live epoch — not the
     base dir (which holds the per-epoch trees). *)
  | Ok profile ->
    Option.map (fun d -> Fpath.(v d / "html-live")) profile.html_dir
  | Error _ -> None

let docs_index_path ~html_dir pkg version =
  Fpath.(html_dir / "p" / pkg / version / "doc" / "index.html")

let docs_exist ~html_dir pkg version =
  Sys.file_exists (Fpath.to_string (docs_index_path ~html_dir pkg version))

(* Compute [(name, version) → (status, hash)] from dag.json + layer
   state. Sources from [dag.json] + layer state so cached nodes
   still appear (the legacy [packages/] dir is only the dispatched-
   this-run subset). Falls back to per-package history when dag.json
   is missing. Slow — typically ≥1s per snapshot because dag.json
   is ~25 MB; callers should go through [load_snapshot_pkgs] which
   adds an on-disk summary cache.

   The aggregation considers ALL kinds for a (name, version) — the
   build, the doc-side compile, and link / doc_all — not just the
   build itself. A package whose build succeeded but whose link
   cascaded due to an upstream doc-side failure correctly shows up
   as "cascade" rather than "success". The chosen [hash] points at
   the node matching the dominant status, so a "cascade" badge
   deep-links to the actually-cascaded link / doc_all node, not to
   the (uninteresting) successful build. *)
let compute_snapshot_pkgs ~os_dir snapshot_dir =
  match read_dag_cached snapshot_dir, os_dir with
  | Ok entries, Some od ->
    let status_index = load_layer_status_cached od in
    let table = Day11_lib.Cascade.classify_from_layer_index
      ~status_index entries in
    let by_pkg : (string * string,
                  (Day11_lib.Cascade.status * string) list) Hashtbl.t =
      Hashtbl.create 4096 in
    List.iter (fun (e : Day11_lib.Dag_marshal.entry) ->
      match e.kind, split_pkg (OpamPackage.to_string e.pkg) with
      | (Build | Compile | Link | Doc_all), Some (n, v) ->
        let st = match Hashtbl.find_opt table e.hash with
          | Some r -> r.status
          | None -> Day11_lib.Cascade.Pending
        in
        let prev = try Hashtbl.find by_pkg (n, v)
          with Not_found -> [] in
        Hashtbl.replace by_pkg (n, v) ((st, e.hash) :: prev)
      | _ -> ()
    ) entries;
    Hashtbl.fold (fun key entries acc ->
      let agg = aggregate_pkg_status_with_hash entries in
      (key, agg) :: acc) by_pkg []
  | _ ->
    snapshot_packages snapshot_dir
    |> List.fold_left (fun m pkg ->
      let entries = Day11_lib.History.read
        ~packages_dir:Fpath.(snapshot_dir / "packages") ~pkg_str:pkg in
      match entries, split_pkg pkg with
      | latest :: _, Some (n, v) ->
        let hash = match latest.build_hash with s -> s in
        ((n, v), (latest.status, hash)) :: m
      | _ -> m
    ) []

(* On-disk summary cache. Stored next to dag.json as
   [pkgs_summary.v3.json], keyed by dag.json's mtime. The summary
   is a small (~250 KB) JSON file mapping [(name, version) →
   (status, hash)]; reading it skips the 25 MB JSON parse +
   classify_from_layers walk that dominates [compute_snapshot_pkgs]
   (~1 s each). Cache is invalidated by dag.json changing, by the
   format version bumping, or by the summary file being deleted.
   Layer state changes (failures flipping to ok via rebuild) are
   NOT detected — the summary records the state at the moment of
   first read; refresh by deleting the summary file if you need a
   re-classify.

   v3: aggregates status across build + compile + link + doc_all
       node kinds. Older summaries only saw the build kind so a
       package whose build was OK but whose docs cascaded showed
       as success.
   v2: added [h] field per entry for deep-linking.
   v1: build status only. *)
let summary_path snapshot_dir =
  Fpath.(snapshot_dir / "pkgs_summary.v3.json")

let dag_mtime snapshot_dir =
  try Some (Unix.stat
    (Fpath.to_string Fpath.(snapshot_dir / "dag.json"))).Unix.st_mtime
  with _ -> None

let read_summary snapshot_dir =
  match Bos.OS.File.read (summary_path snapshot_dir) with
  | Error _ -> None
  | Ok s ->
    try
      let json = Yojson.Safe.from_string s in
      let open Yojson.Safe.Util in
      let dag_mtime_in_file =
        try Some (json |> member "dag_mtime" |> to_number)
        with _ -> None
      in
      let actual = dag_mtime snapshot_dir in
      match dag_mtime_in_file, actual with
      | Some a, Some b when abs_float (a -. b) < 0.5 ->
        let pkgs =
          json |> member "pkgs" |> to_list
          |> List.map (fun e ->
            let n = e |> member "n" |> to_string in
            let v = e |> member "v" |> to_string in
            let s = e |> member "s" |> to_string in
            let h =
              try e |> member "h" |> to_string with _ -> "" in
            ((n, v), (s, h)))
        in
        Some pkgs
      | _ -> None
    with _ -> None

let write_summary snapshot_dir pkgs =
  let dag_mtime = dag_mtime snapshot_dir |> Option.value ~default:0.0 in
  let json : Yojson.Safe.t = `Assoc [
    "dag_mtime", `Float dag_mtime;
    "pkgs", `List (List.map (fun ((n, v), (s, h)) ->
      `Assoc [ "n", `String n; "v", `String v;
               "s", `String s; "h", `String h ]) pkgs);
  ] in
  ignore (Bos.OS.File.write (summary_path snapshot_dir)
            (Yojson.Safe.to_string json))

(* [(name, version) → (status, build_hash)] with on-disk caching. *)
let load_snapshot_pkgs ~os_dir snapshot_dir =
  match read_summary snapshot_dir with
  | Some pkgs -> pkgs
  | None ->
    let pkgs = compute_snapshot_pkgs ~os_dir snapshot_dir in
    write_summary snapshot_dir pkgs;
    pkgs

(* Blessed-package status table written once per completed run
   ([final_status.json], see {!Day11_lib.Status_index.write_final_status}).
   This is the preferred diff source: a small [(name.version -> status)]
   read, blessed-only — exactly the granularity the diffs compare — with
   no 25 MB dag.json parse or layer walk. Categories are mapped to the
   diff vocabulary (success / failure / cascade); there's no per-node
   build hash, so a changed row renders its status badge without a
   deep-link (the version cell still links to the package's per-version
   page). Returns [None] when the file is absent (a snapshot that
   predates it), so callers can fall back. *)
let diff_status_of_category = function
  | "success" | "doc_success" -> "success"
  | "dependency_failure" | "doc_dependency_failure" -> "cascade"
  | _ -> "failure"  (* doc_failure, build_failure, or anything unknown *)

let load_final_status snapshot_dir =
  match Bos.OS.File.read Fpath.(snapshot_dir / "final_status.json") with
  | Error _ -> None
  | Ok s ->
    match (try Some (Yojson.Safe.from_string s) with _ -> None) with
    | Some (`Assoc entries) ->
      Some (List.filter_map (fun (pkgver, st) ->
        match st, split_pkg pkgver with
        | `String status, Some (n, v) ->
          Some ((n, v), (diff_status_of_category status, ""))
        | _ -> None) entries)
    | _ -> None

(* Diff source for a snapshot: prefer [final_status.json]; fall back to
   the dag.json + layer_status classification for older snapshots that
   don't have it. *)
let load_diff_pkgs ~os_dir snapshot_dir =
  match load_final_status snapshot_dir with
  | Some pkgs -> pkgs
  | None -> load_snapshot_pkgs ~os_dir snapshot_dir

(* Per-process memo of [load_diff_pkgs] keyed by snapshot dir.
   Snapshot dirs are append-mostly + content-addressed by mtime, so
   for a single page render a hit on the same dir always returns the
   right value. Lifetime is the closure that owns the [Hashtbl] —
   one per request. *)
let make_load_snapshot_pkgs_memo ~os_dir =
  let cache : (string,
               ((string * string) * (string * string)) list) Hashtbl.t =
    Hashtbl.create 32 in
  fun snapshot_dir ->
    let key = Fpath.to_string snapshot_dir in
    match Hashtbl.find_opt cache key with
    | Some v -> v
    | None ->
      let v = load_diff_pkgs ~os_dir snapshot_dir in
      Hashtbl.add cache key v;
      v

(* Diff [m_old] against [m_new] (both [(name, version) → status]),
   collapsing single-old-vs-single-new pairs of the same name into
   a [Version_changed] row (typical for latest-only profiles where
   a version bump replaces the prior version).

   The two inputs are converted to Hashtbls up-front so the inner
   per-key lookup is O(1); a profile with ~10 K packages has ~10 K
   keys, so the prior [List.assoc_opt] approach was O(n²) per
   diff and dominated the page render. *)
let compute_diff_changes m_old m_new =
  let to_table m =
    let t = Hashtbl.create (List.length m * 2) in
    List.iter (fun (k, v) -> Hashtbl.replace t k v) m;
    t
  in
  let t_old = to_table m_old and t_new = to_table m_new in
  let keys =
    let s = Hashtbl.create (Hashtbl.length t_old + Hashtbl.length t_new) in
    Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) t_old;
    Hashtbl.iter (fun k _ -> Hashtbl.replace s k ()) t_new;
    Hashtbl.fold (fun k () acc -> k :: acc) s [] |> List.sort compare
  in
  let classify =
    List.filter_map (fun (n, v) ->
      let old_e = Hashtbl.find_opt t_old (n, v) in
      let new_e = Hashtbl.find_opt t_new (n, v) in
      match old_e, new_e with
      | None, None -> None
      | None, Some (sn, hn) -> Some (n, `Added (v, sn, hn))
      | Some _, None -> Some (n, `Removed v)
      | Some (so, _), Some (sn, _) when so = sn -> None
      | Some (so, _), Some (sn, hn) ->
        Some (n, `Status_changed (v, so, sn, hn))
    ) keys
  in
  let counts : (string, int ref * int ref) Hashtbl.t =
    Hashtbl.create 16 in
  List.iter (fun (n, k) ->
    let r, a = try Hashtbl.find counts n
      with Not_found ->
        let cell = (ref 0, ref 0) in
        Hashtbl.add counts n cell;
        cell
    in
    match k with
    | `Removed _ -> incr r
    | `Added _ -> incr a
    | `Status_changed _ -> ()
  ) classify;
  let paired : (string, string * string * string * string) Hashtbl.t =
    Hashtbl.create 16 in
  List.iter (fun (n, k) ->
    match Hashtbl.find_opt counts n with
    | Some (r, a) when !r = 1 && !a = 1 ->
      let existing = Hashtbl.find_opt paired n in
      (match k, existing with
       | `Removed v_old, None ->
         Hashtbl.add paired n (v_old, "", "", "")
       | `Removed v_old, Some (_, v_new, s_new, h_new) ->
         Hashtbl.replace paired n (v_old, v_new, s_new, h_new)
       | `Added (v_new, s_new, h_new), None ->
         Hashtbl.add paired n ("", v_new, s_new, h_new)
       | `Added (v_new, s_new, h_new), Some (v_old, _, _, _) ->
         Hashtbl.replace paired n (v_old, v_new, s_new, h_new)
       | _ -> ())
    | _ -> ()
  ) classify;
  List.filter_map (fun (n, k) ->
    match k, Hashtbl.find_opt paired n with
    | `Removed _, Some _ -> None  (* collapsed into the added row *)
    | `Added (_, _, _), Some (v_old, v_new, s_new, h_new)
      when v_old <> "" && v_new <> "" ->
      Some (n, Version_changed (v_old, v_new, s_new, h_new))
    | `Added (v, sn, hn), _ -> Some (n, Added (v, sn, hn))
    | `Removed v, _ -> Some (n, Removed v)
    | `Status_changed (v, so, sn, hn), _ ->
      Some (n, Status_changed (v, so, sn, hn))
  ) classify

(* "Newly broken" filter for the recent-changes page. Keeps only
   rows whose new status is failure or cascade — i.e. the on-call
   view of what just stopped working. *)
let is_change_failure = function
  | Added (_, ("failure" | "cascade"), _) -> true
  | Status_changed (_, _, ("failure" | "cascade"), _) -> true
  | Version_changed (_, _, ("failure" | "cascade"), _) -> true
  | _ -> false

(* TyXML <tr> for one diff row. Used by both [snapshot_diff] and
   [recent_changes]. Cells link out wherever they can:

   - package name → [/profiles/<name>/p/<pkg>] (cross-snapshot
     version index for this package);
   - version → [/profiles/<name>/p/<pkg>/<ver>] (per-version
     history page with rebuild info);
   - status badge → docs (success) or build log (failure / cascade)
     — the build log link prefers the OCurrent job page when we can
     find the job_id, otherwise falls back to the raw [layer.log]
     viewer at [/profiles/<name>/builds/<hash>/log]. *)
let render_change_row ~profile_name ~html_dir (name, change) =
  let pkg_link =
    a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s" profile_name name) ]
      [ txt name ]
  in
  let ver_link v =
    a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s/%s"
                     profile_name name v) ]
      [ txt v ]
  in
  let status_cell ~version status hash =
    let span = Templates.status_span status in
    match status with
    | "success" when version <> "" ->
      (* Only link to docs when the rendered index.html is actually
         on disk. voodoo can fail for individual packages even when
         the build layer succeeded — the [<html_dir>/p/<pkg>/<ver>/]
         tree just won't exist. Linking anyway gives a 404. *)
      (match html_dir with
       | Some h when docs_exist ~html_dir:h name version ->
         a ~a:[ a_href (Printf.sprintf
                          "/profiles/%s/docs/p/%s/%s/doc/index.html"
                          profile_name name version) ]
           [ span ]
       | _ -> span)
    | ("failure" | "cascade") when hash <> "" ->
      let target = match job_id_for_hash hash with
        | Some job_id -> "/job/" ^ job_id
        | None ->
          Printf.sprintf "/profiles/%s/builds/%s/log" profile_name hash
      in
      a ~a:[ a_href target ] [ span ]
    | _ -> span
  in
  match change with
  | Added (v, sn, hn) ->
    tr [ td [ pkg_link ]; td [ ver_link v ];
         td [ status_cell ~version:v sn hn ];
         td [ em [ txt "added" ] ] ]
  | Removed v ->
    tr [ td [ pkg_link ]; td [ txt v ];
         td []; td [ em [ txt "removed" ] ] ]
  | Status_changed (v, so, sn, hn) ->
    tr [ td [ pkg_link ]; td [ ver_link v ];
         td [ Templates.status_span so; txt " → ";
              status_cell ~version:v sn hn ];
         td [ em [ txt "status changed" ] ] ]
  | Version_changed (v_old, v_new, s_new, h_new) ->
    tr [ td [ pkg_link ];
         td [ ver_link v_old; txt " → "; ver_link v_new ];
         td [ status_cell ~version:v_new s_new h_new ];
         td [ em [ txt "version changed" ] ] ]

let diff_table_thead =
  thead [ tr [ th [ txt "Package" ];
               th [ txt "Version" ];
               th [ txt "Status" ];
               th [ txt "Change" ] ] ]

(* ── /profiles/<name>/snapshots/<key>/diff/<other> ────────────── *)

(* The diff page must ignore "pending" rows: their status isn't
   final, so a pending entry would otherwise show as "removed" (when
   the prior version was OK and the new one isn't ready yet) or
   "added" (the mirror case). The right semantics is symmetric and
   keyed by {b package name} — version bumps in a single profile go
   together with pending while the new version builds, so dropping by
   [(name, version)] still misclassifies the old version as removed.
   If any version of a name is pending in either snapshot, every
   version of that name is excluded from both sides until the
   pending one resolves. Per-side counts are still surfaced in the
   incomplete-snapshot banner so the reader knows which snapshot is
   still settling. *)
let pending_pkg_names pkgs =
  List.fold_left (fun acc ((n, _), (st, _)) ->
    if st = "pending" then n :: acc else acc) [] pkgs

(* [(repo_path, commit)] recorded in a snapshot's repos.json. *)
let snapshot_repo_commits dir =
  match Day11_batch.Snapshot.load dir with
  | Ok s -> s.Day11_batch.Snapshot.repos
  | Error _ -> []

(* [git log old..new] for one repo, as (short_hash, date, author, subject)
   rows. Read-only, bounded; returns [] when the range is empty or can't
   be read (missing repo, equal commits, force-push / unrelated history).
   Fields are split on US (0x1f), which never appears in commit metadata. *)
let git_log_range ~repo ~old_commit ~new_commit =
  if old_commit = new_commit || old_commit = "" || new_commit = "" then []
  else
    let cmd =
      Printf.sprintf
        "git -C %s log -n 500 --no-decorate \
         --pretty=format:%%h%%x1f%%ad%%x1f%%an%%x1f%%s --date=short %s 2>/dev/null"
        (Filename.quote repo)
        (Filename.quote (old_commit ^ ".." ^ new_commit))
    in
    let ic = Unix.open_process_in cmd in
    let rec loop acc =
      match input_line ic with
      | line -> loop (line :: acc)
      | exception End_of_file -> List.rev acc
    in
    let lines = loop [] in
    ignore (Unix.close_process_in ic);
    List.filter_map (fun l ->
      match String.split_on_char '\x1f' l with
      | [ h; d; a; s ] -> Some (h, d, a, s)
      | _ -> None) lines

(* Per-repo git-log section for the diff page: the opam-repository commits
   between the two snapshots' recorded HEADs. *)
let repo_changes_section ~dir_old ~dir_new =
  (* Read the log oldest -> newest regardless of which snapshot is the
     page's base vs target (created is ISO-8601, so string compare is
     chronological). *)
  let dir_old, dir_new =
    if snapshot_created dir_old <= snapshot_created dir_new
    then dir_old, dir_new else dir_new, dir_old in
  let old_commits = snapshot_repo_commits dir_old in
  List.concat_map (fun (path, new_commit) ->
    match List.assoc_opt path old_commits with
    | None -> []
    | Some old_commit ->
      let repo_name = Filename.basename path in
      let rows = git_log_range ~repo:path ~old_commit ~new_commit in
      let header =
        h3 [ txt (Printf.sprintf "%s  %s..%s" repo_name
                    (Templates.short_sha old_commit)
                    (Templates.short_sha new_commit)) ] in
      let body =
        match rows with
        | [] ->
          [ p [ em [ txt "No commits in this range (the newer snapshot's \
                          HEAD is not a descendant of the older one, or the \
                          repo is unavailable)." ] ] ]
        | _ ->
          let truncated =
            if List.length rows >= 500 then
              [ p [ em [ txt "(showing first 500 commits)" ] ] ] else [] in
          truncated @
          [ table ~a:[ a_class [ "data" ] ]
              ~thead:(thead [ tr [ th [ txt "Commit" ]; th [ txt "Date" ];
                                   th [ txt "Author" ]; th [ txt "Subject" ] ] ])
              (List.map (fun (h, d, a, s) ->
                 tr [ td [ Templates.sha_span h ]; td [ txt d ];
                      td [ txt a ]; td [ txt s ] ]) rows) ]
      in
      header :: body
  ) (snapshot_repo_commits dir_new)

let snapshot_diff ~ctx name key_old key_new =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let dir_old = Fpath.(snapshots_base ctx name / key_old) in
      let dir_new = Fpath.(snapshots_base ctx name / key_new) in
      let os_dir = os_dir_for ~ctx name in
      let html_dir = html_dir_for ~ctx name in
      let load = make_load_snapshot_pkgs_memo ~os_dir in
      let m_old_raw = load dir_old and m_new_raw = load dir_new in
      let p_old = pending_pkg_names m_old_raw
      and p_new = pending_pkg_names m_new_raw in
      let pending_old = List.length p_old
      and pending_new = List.length p_new in
      let drop =
        let t = Hashtbl.create (pending_old + pending_new) in
        List.iter (fun n -> Hashtbl.replace t n ()) p_old;
        List.iter (fun n -> Hashtbl.replace t n ()) p_new;
        t
      in
      let strip = List.filter (fun ((n, _), _) -> not (Hashtbl.mem drop n)) in
      let m_old = strip m_old_raw and m_new = strip m_new_raw in
      let changes = compute_diff_changes m_old m_new in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/snapshots" name), "Snapshots";
        Some (Printf.sprintf "/profiles/%s/snapshots/%s" name key_old),
          Templates.short_sha key_old;
        None, "diff " ^ Templates.short_sha key_new;
      ] in
      let incomplete_notice =
        let mk label key n =
          Printf.sprintf "%s snapshot %s is incomplete: %d package%s still pending"
            label (Templates.short_sha key) n (if n = 1 then "" else "s")
        in
        match pending_old, pending_new with
        | 0, 0 -> []
        | _, 0 -> [ p ~a:[ a_class [ "warn" ] ]
                     [ txt (mk "Old" key_old pending_old) ] ]
        | 0, _ -> [ p ~a:[ a_class [ "warn" ] ]
                     [ txt (mk "New" key_new pending_new) ] ]
        | _, _ ->
          [ p ~a:[ a_class [ "warn" ] ]
              [ txt (mk "Old" key_old pending_old) ];
            p ~a:[ a_class [ "warn" ] ]
              [ txt (mk "New" key_new pending_new) ] ]
      in
      let body =
        if changes = [] then [ p [ em [ txt "No differences." ] ] ]
        else [ table ~a:[ a_class [ "data" ] ]
                 ~thead:diff_table_thead
                 (List.map (render_change_row ~profile_name:name ~html_dir) changes) ]
      in
      (* opam-repository commits between the two snapshots' recorded HEADs. *)
      let repo_section =
        h2 [ txt "Repository commits" ]
        :: repo_changes_section ~dir_old ~dir_new
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt (Printf.sprintf "%s — diff" name) ];
        p [ txt "From "; Templates.sha_span key_old;
            txt " to "; Templates.sha_span key_new ];
      ] @ incomplete_notice
        @ (h2 [ txt "Package changes" ] :: body)
        @ repo_section)
  end

(* ── /profiles/<name>/recent[?n=K&page=N&status=fail|change] ──── *)

(* Default window: how many snapshot pairs to walk on one page.
   Each pair reads two snapshot dirs (memoized so consecutive pairs
   share one load), so 20 pairs ≈ 21 unique snapshot reads. *)
let recent_default_n = 20

let recent_changes ~ctx name =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let req = Context.request web_ctx in
      let uri = Cohttp.Request.uri req in
      let n =
        match Uri.get_query_param uri "n" with
        | Some s -> (try max 1 (min 200 (int_of_string s))
                     with _ -> recent_default_n)
        | None -> recent_default_n
      in
      let page =
        match Uri.get_query_param uri "page" with
        | Some s -> (try max 1 (int_of_string s) with _ -> 1)
        | None -> 1
      in
      let status_filter =
        match Uri.get_query_param uri "status" with
        | Some "fail" -> `Fail
        | _ -> `Change
      in
      let snaps = list_snapshots_newest_first ctx name in
      let total_pairs = max 0 (List.length snaps - 1) in
      let n_pages = max 1 ((total_pairs + n - 1) / n) in
      let page = min page n_pages in
      let start_pair = (page - 1) * n in
      (* For pairs [start_pair .. start_pair + n - 1] we need
         snapshots [start_pair .. start_pair + n] (one extra for
         the older end of the last pair). *)
      let visible_snaps =
        snaps
        |> List.filteri (fun i _ ->
          i >= start_pair && i <= start_pair + n)
      in
      let os_dir = os_dir_for ~ctx name in
      let html_dir = html_dir_for ~ctx name in
      let load = make_load_snapshot_pkgs_memo ~os_dir in
      let mtime_str dir =
        try
          let s = Unix.stat (Fpath.to_string dir) in
          let tm = Unix.gmtime s.st_mtime in
          Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
            tm.tm_hour tm.tm_min
        with _ -> "—"
      in
      (* Walk visible snapshots newest-to-oldest. Each adjacent
         pair (dir_new, dir_old) becomes a section; sections with
         no changes after filtering are skipped entirely. *)
      let rec pairs acc = function
        | dir_new :: (dir_old :: _ as rest) ->
          pairs ((dir_new, dir_old) :: acc) rest
        | _ -> List.rev acc
      in
      let sections =
        pairs [] visible_snaps
        |> List.filter_map (fun (dir_new, dir_old) ->
          let changes = compute_diff_changes (load dir_old) (load dir_new) in
          let changes = match status_filter with
            | `Change -> changes
            | `Fail -> List.filter (fun (_, c) -> is_change_failure c) changes
          in
          if changes = [] then None
          else
            let key_new = Fpath.basename dir_new in
            let key_old = Fpath.basename dir_old in
            let header =
              h3 [
                a ~a:[ a_href (Printf.sprintf
                                 "/profiles/%s/snapshots/%s" name key_new) ]
                  [ Templates.sha_span key_new ];
                txt (" — " ^ mtime_str dir_new ^ " ");
                a ~a:[ a_href (Printf.sprintf
                                 "/profiles/%s/snapshots/%s/diff/%s"
                                 name key_old key_new) ]
                  [ txt "(full diff)" ];
              ]
            in
            let table_el =
              table ~a:[ a_class [ "data" ] ]
                ~thead:diff_table_thead
                (List.map (render_change_row ~profile_name:name ~html_dir) changes)
            in
            Some [ header; table_el ])
        |> List.concat
      in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "Recent changes";
      ] in
      let filter_link ?(label = "") which =
        let href = Printf.sprintf "/profiles/%s/recent?n=%d&status=%s"
          name n which in
        let lbl = if label = "" then which else label in
        if (which = "change" && status_filter = `Change)
           || (which = "fail" && status_filter = `Fail)
        then b [ txt lbl ]
        else a ~a:[ a_href href ] [ txt lbl ]
      in
      let filters = p ~a:[ a_class [ "crumbs" ] ] [
        txt "Show: ";
        filter_link ~label:"all changes" "change";
        txt " · ";
        filter_link ~label:"only newly failing" "fail";
      ] in
      let pager =
        if n_pages <= 1 then []
        else
          let link p_num label =
            a ~a:[ a_href (Printf.sprintf
                             "/profiles/%s/recent?n=%d&status=%s&page=%d"
                             name n
                             (match status_filter with
                              | `Change -> "change" | `Fail -> "fail")
                             p_num) ]
              [ txt label ]
          in
          [ div ~a:[ a_class [ "pager" ] ]
              (List.concat [
                (if page > 1 then [ link (page - 1) "‹ Newer"; txt " " ]
                 else []);
                [ txt (Printf.sprintf "Page %d of %d (%d snapshot pairs)"
                         page n_pages total_pairs) ];
                (if page < n_pages then [ txt " "; link (page + 1) "Older ›" ]
                 else []);
              ]) ]
      in
      let body =
        if sections = [] then
          [ p [ em [ txt "No changes in this window." ] ] ]
        else sections
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt (name ^ " — recent changes") ];
           filters ]
         @ body @ pager)
  end

(* ── /profiles/<name>/p/<pkg> ─────────────────────────────────── *)

let package_index ~ctx name pkg =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      (* Find versions of [pkg] across all snapshots. *)
      let snaps = list_snapshots_newest_first ctx name in
      let versions = List.fold_left (fun acc snap ->
        let pdir = Fpath.(snap / "packages") in
        match Bos.OS.Dir.contents pdir with
        | Error _ -> acc
        | Ok entries ->
          List.fold_left (fun acc p ->
            let basename = Fpath.basename p in
            if String.length basename > String.length pkg + 1
               && String.sub basename 0 (String.length pkg + 1)
                  = pkg ^ "." then
              let v = String.sub basename (String.length pkg + 1)
                (String.length basename - String.length pkg - 1) in
              if List.mem v acc then acc else v :: acc
            else acc) acc entries
      ) [] snaps |> List.sort compare in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "package: " ^ pkg;
      ] in
      let body = match versions with
        | [] -> [ p [ em [ txt "No builds of this package in any \
                                snapshot." ] ] ]
        | _ ->
          [ ul (List.map (fun v ->
              li [ a ~a:[ a_href (Printf.sprintf
                                    "/profiles/%s/p/%s/%s" name pkg v) ]
                     [ txt (pkg ^ "." ^ v) ] ]) versions) ]
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt (name ^ " / " ^ pkg) ] ] @ body)
  end

(* ── /profiles/<name>/p/<pkg>/<ver> ───────────────────────────── *)

(** Percent-encode a string for use in a URL query (GitHub issue
    title/body/search). Conservative: only unreserved chars pass
    through, everything else (incl. newlines and spaces) is %XX. *)
let urlencode s =
  let buf = Buffer.create (String.length s * 3) in
  String.iter (fun c ->
    match c with
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
      Buffer.add_char buf c
    | c -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c)))
    s;
  Buffer.contents buf

(** Substring test without allocation (used to pre-filter big
    [build.jsonl] lines before JSON-parsing). *)
let str_contains s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then true
  else
    let rec at i j =
      if j = m then true
      else if i + j >= n then false
      else if s.[i + j] = sub.[j] then at i (j + 1)
      else false
    in
    let rec loop i = if i + m > n then false else if at i 0 then true else loop (i + 1) in
    loop 0

(** Minimal HTML escape for text interpolated into raw-HTML
    [Unsafe.data] fragments (dep names, hashes). *)
let esc_html s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> match c with
    | '&' -> Buffer.add_string b "&amp;"
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c) s;
  Buffer.contents b

(* A persisted solver failure for [pkg_str] in [snapshot_dir], if the
   solve of that target failed (no dependency solution was found). Read
   from [solutions/<pkg>.<ver>.json] ([failed:true] + the solver's
   [error]). [None] when the package solved or there's no solve record. *)
let read_solver_failure snapshot_dir pkg_str =
  match Bos.OS.File.read
          Fpath.(snapshot_dir / "solutions" / (pkg_str ^ ".json")) with
  | Error _ -> None
  | Ok data ->
    match (try Some (Yojson.Safe.from_string data) with _ -> None) with
    | Some (`Assoc _ as j) ->
      let open Yojson.Safe.Util in
      (match j |> member "failed" |> to_bool_option with
       | Some true ->
         (match j |> member "error" |> to_string_option with
          | Some e when String.trim e <> "" -> Some e
          | _ -> Some "(solve failed; no error message recorded)")
       | _ -> None)
    | _ -> None

let package_version ~ctx name pkg ver =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let pkg_str = pkg ^ "." ^ ver in
      let snaps = list_snapshots_newest_first ctx name in
      (* [read_latest] dedupes by build_hash: one entry per unique
         (build/compile/doc_all/link) layer hash, keeping the most
         recent. Without dedup the table grows linearly with retries
         and snapshots, mostly repeats.

         Universe + blessing are read straight off the history entry
         (persisted at build time from the in-memory plan). The page
         therefore never parses dag.json — which for the full profile is
         hundreds of MB and dominated by per-node dep lists we don't
         need here. Legacy entries written before [universe] existed
         carry "" and render as "—". *)
      let entries = List.concat_map (fun snap ->
        let pdir = Fpath.(snap / "packages") in
        Day11_lib.History.read_latest ~packages_dir:pdir ~pkg_str
      ) snaps in
      (* Fallback for a package this profile *planned* but never
         dispatched under its own name — a shared dep another profile
         built, or a build that failed: per-profile [history.jsonl] has
         nothing, yet the plan-time [packages/<pkg>.<ver>/plan.json]
         records the node hashes. Synthesise entries from the plan and
         resolve each hash's outcome from the shared [layer_status.jsonl]
         so the page shows real status + a job link instead of "No
         history entries". See doc/package-status-plan-records.md. *)
      let entries =
        if entries <> [] then entries
        else begin
          let ls = match os_dir_for ~ctx name with
            | Some od -> load_layer_status_cached od
            | None -> Hashtbl.create 1 in
          let short h = String.sub h 0 (min 12 (String.length h)) in
          let seen : (string, unit) Hashtbl.t = Hashtbl.create 16 in
          List.concat_map (fun snap ->
            let pf = Fpath.(snap / "packages" / pkg_str / "plan.json") in
            match Bos.OS.File.read pf with
            | Error _ -> []
            | Ok data ->
              match (try Some (Yojson.Safe.from_string data) with _ -> None) with
              | Some (`List nodes) ->
                let open Yojson.Safe.Util in
                let parsed = List.filter_map (fun j ->
                  match j |> member "hash" |> to_string_option with
                  | None -> None
                  | Some hash ->
                    let kind = j |> member "kind" |> to_string_option
                               |> Option.value ~default:"build" in
                    let universe = j |> member "universe" |> to_string_option
                                   |> Option.value ~default:"" in
                    let blessed = j |> member "blessed" |> to_bool_option
                                  |> Option.value ~default:false in
                    Some (hash, kind, universe, blessed)) nodes in
                (* Build nodes carry per-universe [blessed=false] in the plan;
                   the recorder instead marks a build entry blessed when it's
                   the blessed *version* of the package. Mirror that so the
                   diagnostic banner attributes a build failure to the build,
                   not the (pending) docs. *)
                let pkg_blessed = List.exists (fun (_,_,_,b) -> b) parsed in
                List.filter_map (fun (hash, kind, universe, blessed) ->
                  if Hashtbl.mem seen hash then None
                  else begin
                    Hashtbl.replace seen hash ();
                    let is_doc =
                      kind = "doc_all" || kind = "link" || kind = "compile" in
                    let exit_opt =
                      match Hashtbl.find_opt ls (short hash) with
                      | Some (e : Day11_layer.Layer_status.entry) ->
                        Some e.exit_status
                      | None -> None in
                    let status, category = match exit_opt with
                      | Some 0 ->
                        "success", (if is_doc then "doc_success" else "success")
                      | Some _ ->
                        "failure",
                        (if is_doc then "doc_failure" else "build_failure")
                      | None ->
                        "pending", (if is_doc then "doc" else "build") in
                    let blessed = if is_doc then blessed else pkg_blessed in
                    Some { Day11_lib.History.ts = ""; run = "(plan)";
                           build_hash = hash; status; category; blessed;
                           error = None; universe }
                  end) parsed
              | _ -> []
          ) snaps
        end
      in
      (* One batched SQL query for all build_hashes' job_ids, instead of
         opening + querying + closing the OCurrent cache db once per
         history entry (a package like odoc has ~hundreds of entries —
         that per-entry loop dominated render time and thrashed the db
         under the live daemon's writes). *)
      let job_ids =
        job_ids_for_hashes
          (List.map (fun (e : Day11_lib.History.entry) -> e.build_hash)
             entries) in
      let job_id_of bh =
        if String.length bh = 0 then None
        else Hashtbl.find_opt job_ids (String.sub bh 0 (min 12 (String.length bh)))
      in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        Some (Printf.sprintf "/profiles/%s/p/%s" name pkg),
          "package: " ^ pkg;
        None, ver;
      ] in
      (* Map build_hash -> transitive build-deps for this version. The
         closure is recorded in each build layer's [build.json]
         ([Build_meta.t.build_deps]); layers are content-addressed and
         shared across snapshots, so one [Build_meta.load] per distinct
         build_hash (keyed off [os_dir]) suffices — no snapshot sweep.
         Doc nodes and pre-[build_deps] layers yield [], so their hashes
         are simply absent from the table. Drives the inline "Deps"
         column and the compare control. *)
      let build_deps_map =
        let tbl : (string, string list) Hashtbl.t = Hashtbl.create 64 in
        (match os_dir_for ~ctx name with
         | None -> ()
         | Some os_dir ->
           List.iter (fun (e : Day11_lib.History.entry) ->
             if String.length e.build_hash > 0
                && not (Hashtbl.mem tbl e.build_hash) then
               let layer_dir =
                 Day11_layer.Layer.(dir (of_hash ~os_dir e.build_hash)) in
               match Day11_opam_layer.Build_meta.load layer_dir with
               | Ok bm when bm.build_deps <> [] ->
                 Hashtbl.replace tbl e.build_hash bm.build_deps
               | _ -> ())
             entries);
        tbl
      in
      let history_rows = List.map (fun (e : Day11_lib.History.entry) ->
        (* Prefer linking to the OCurrent job page (gives a Rebuild
           button and structured log) when we can find it; fall back
           to the raw layer.log file when the cache no longer has the
           job_id (e.g. for entries pre-dating the SQLite cache). *)
        let hash_cell =
          let target = match job_id_of e.build_hash with
            | Some job_id -> "/job/" ^ job_id
            | None ->
              Printf.sprintf "/profiles/%s/builds/%s/log"
                name e.build_hash
          in
          a ~a:[ a_href target ] [ Templates.sha_span e.build_hash ]
        in
        let error_cell = match e.error with
          | Some err -> [ code [ txt err ] ]
          | None -> []
        in
        (* Doc entries carry category "doc_success"/"doc_failure"; build
           entries are "success" or a build_* failure category. *)
        let is_doc_entry =
          String.length e.category >= 3
          && String.sub e.category 0 3 = "doc"
        in
        (* Category is just the node kind: "docs" or "build". The
           outcome is already in the Status column and any failure
           detail in Error. *)
        let category_cell = txt (if is_doc_entry then "docs" else "build") in
        let universe = e.universe and blessed = e.blessed in
        (* Universe (doc nodes) links to the universe page. Read from the
           history entry — see the note above. "" => "—". *)
        let universe_cell =
          if universe <> "" then
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/u/%s" name universe) ]
              [ Templates.sha_span universe ]
          else em [ txt "—" ]
        in
        (* The blessed flag is recorded on both build and doc nodes (the
           node's universe is the blessed one for this package), so show
           it on every row that carries it — not just doc rows. *)
        let blessed_cell =
          if blessed then span ~a:[ a_class [ "ok" ] ] [ txt "blessed" ]
          else em [ txt "—" ]
        in
        (* Inline build-deps: a folded [<details>] of the transitive
           build-deps, plus a checkbox to pick this build for the
           compare tool above the table. Build rows only. *)
        let deps_cell =
          if is_doc_entry then td [ em [ txt "—" ] ]
          else
            match Hashtbl.find_opt build_deps_map e.build_hash with
            | None -> td [ em [ txt "—" ] ]
            | Some deps ->
              let deps_li =
                String.concat ""
                  (List.map (fun d -> "<li>" ^ esc_html d ^ "</li>") deps) in
              td [ Unsafe.data (Printf.sprintf
                "<label title=\"select to compare\">\
                 <input type=\"checkbox\" class=\"bd-sel\" value=\"%s\"> </label>\
                 <details><summary>%d deps</summary><ul>%s</ul></details>"
                (esc_html e.build_hash) (List.length deps) deps_li) ]
        in
        tr [ td [ txt e.ts ];
             td [ txt e.run ];
             td [ Templates.status_span e.status ];
             td [ category_cell ];
             td [ universe_cell ];
             td [ blessed_cell ];
             td [ hash_cell ];
             deps_cell;
             td error_cell ]
      ) entries in
      let history_block = match history_rows with
        | [] -> [ p [ em [ txt "No history entries." ] ] ]
        | _ ->
          [ table ~a:[ a_class [ "data" ] ]
              ~thead:(thead [ tr [ th [ txt "Time" ];
                                   th [ txt "Run" ];
                                   th [ txt "Status" ];
                                   th [ txt "Category" ];
                                   th [ txt "Universe" ];
                                   th [ txt "Blessed" ];
                                   th [ txt "Hash" ];
                                   th [ txt "Deps" ];
                                   th [ txt "Error" ] ] ])
              history_rows ]
      in
      (* Diagnostic blurb: when docs aren't available, explain why —
         build failure vs doc-generation failure — and link the
         relevant *blessed* job plus a "report this" affordance. Driven
         entirely off the history entries (no dag.json). *)
      let is_doc_cat (e : Day11_lib.History.entry) =
        String.length e.category >= 3 && String.sub e.category 0 3 = "doc"
      in
      let latest_where pred =
        List.filter pred entries
        |> List.sort (fun (a : Day11_lib.History.entry) b ->
             compare b.ts a.ts)
        |> function [] -> None | x :: _ -> Some x
      in
      let blessed_build =
        latest_where (fun (e : Day11_lib.History.entry) ->
          (not (is_doc_cat e)) && e.blessed) in
      let blessed_doc =
        latest_where (fun (e : Day11_lib.History.entry) ->
          is_doc_cat e && e.blessed) in
      let job_link bh label =
        let target = match job_id_of bh with
          | Some job_id -> "/job/" ^ job_id
          | None -> Printf.sprintf "/profiles/%s/builds/%s/log" name bh
        in
        a ~a:[ a_href target ] [ txt label ]
      in
      let os_label =
        match Profile.load ~dir:ctx.profile_dir ~name with
        | Ok (p : Profile.t) ->
          Printf.sprintf "%s %s"
            (String.capitalize_ascii p.os_distribution) p.os_version
        | Error _ -> "this platform"
      in
      let report_affordance ~repo ~title ~body ~lead =
        let new_url = Printf.sprintf
          "https://github.com/%s/issues/new?title=%s&body=%s"
          repo (urlencode title) (urlencode body) in
        let search_url = Printf.sprintf
          "https://github.com/%s/issues?q=%s"
          repo (urlencode ("is:issue " ^ pkg)) in
        p [ txt lead; txt " ";
            a ~a:[ a_href search_url ] [ txt "find issue" ];
            txt " · ";
            a ~a:[ a_href new_url ] [ txt "report issue" ] ]
      in
      (* Absolute URL of this page, so a filed issue links back. Host
         comes from the request; behind Caddy the public scheme is in
         [x-forwarded-proto] (default https). *)
      let page_url =
        let headers = Cohttp.Request.headers (Context.request web_ctx) in
        let path = Printf.sprintf "/profiles/%s/p/%s/%s" name pkg ver in
        match Cohttp.Header.get headers "host" with
        | None | Some "" -> path
        | Some host ->
          let proto = match Cohttp.Header.get headers "x-forwarded-proto" with
            | Some p when p <> "" -> p | _ -> "https" in
          Printf.sprintf "%s://%s%s" proto host path
      in
      let build_fail_phrase = function
        | "depext_unavailable" -> "a missing system dependency"
        | "transient_failure" -> "a transient infrastructure error"
        | _ -> "a build error" in
      (* Cascade attribution. A skipped (cascaded) build leaves no
         history.jsonl entry — only a [build.jsonl] line in the run log:
         {"pkg":..,"status":"cascade","failed_dep":".."}. Find the most
         recent such line for this version, scanning snapshots
         newest-first and (within each) the newest run. Only called on
         the no-history path, so it costs nothing on the happy path. *)
      let latest_cascade_dep () =
        let pkg_needle = Printf.sprintf "\"pkg\":\"%s\"" pkg_str in
        let scan path =
          if not (Sys.file_exists path) then None
          else begin
            let ic = open_in path in
            Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
              let found = ref None in
              (try while true do
                 let line = input_line ic in
                 (* Only the [kind:"build"] cascade names the real root
                    dependency. Doc-node cascades (compile/doc-all/link)
                    depend on this package's own build, so their
                    [failed_dep] is the package itself — skip those. *)
                 if str_contains line pkg_needle
                    && str_contains line "\"status\":\"cascade\""
                    && str_contains line "\"kind\":\"build\"" then
                   (match Yojson.Safe.from_string line with
                    | `Assoc a ->
                      let get k = match List.assoc_opt k a with
                        | Some (`String s) -> Some s | _ -> None in
                      if get "pkg" = Some pkg_str
                         && get "status" = Some "cascade"
                         && get "kind" = Some "build" then
                        (match get "failed_dep" with
                         | Some d when d <> pkg_str -> found := Some d
                         | _ -> ())
                    | _ -> () | exception _ -> ())
               done with End_of_file -> ());
              !found)  (* last match in the file = most recent *)
          end
        in
        let snap_dep snap =
          match Bos.OS.Dir.contents Fpath.(snap / "runs") with
          | Error _ -> None
          | Ok entries ->
            (* Scan runs newest-first: a snapshot's *newest* run may not
               have touched this package (doc-only re-run, partial run),
               while an older one recorded the cascade. *)
            List.map Fpath.to_string entries
            |> List.sort (fun a b -> compare b a)
            |> List.find_map (fun rd ->
                 scan (Filename.concat rd "build.jsonl"))
        in
        List.find_map snap_dep snaps
      in
      let cascade_blurb dep =
        let dep_link =
          match String.index_opt dep '.' with
          | None ->
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s" name dep) ]
              [ txt dep ]
          | Some i ->
            let n = String.sub dep 0 i in
            let v = String.sub dep (i + 1) (String.length dep - i - 1) in
            a ~a:[ a_href (Printf.sprintf "/profiles/%s/p/%s/%s" name n v) ]
              [ txt dep ]
        in
        [ div ~a:[ a_class [ "warn" ] ]
            [ p [ span ~a:[ a_class [ "cascade" ] ] [ txt "⚠ Not built" ];
                  txt (Printf.sprintf
                    " — a dependency of %s failed to build, so it was \
                     skipped (cascade)." pkg_str) ];
              p [ txt "Failing dependency: "; dep_link ];
              p [ txt "This is usually not a problem with "; txt pkg;
                  txt " itself — it should build once the dependency does. \
                       Follow the dependency above to see why it failed." ] ] ]
      in
      let docs_present =
        match html_dir_for ~ctx name with
        | Some h -> docs_exist ~html_dir:h pkg ver
        | None -> false
      in
      let status_block =
        if docs_present then
          [ p [ a ~a:[ a_href (Printf.sprintf
                                 "/profiles/%s/docs/p/%s/%s/doc/index.html"
                                 name pkg ver) ]
                  [ txt "Open rendered docs" ] ] ]
        else
          match blessed_build, blessed_doc with
          | Some bb, _ when bb.status <> "success" ->
            let body = Printf.sprintf
              "Package %s failed to build on %s, so no documentation could \
               be produced.\n\nBuild hash: %s\nCategory: %s\n%sProfile: %s\n\
               Page: %s\n"
              pkg_str os_label bb.build_hash bb.category
              (match bb.error with
               | Some e -> "Error: " ^ e ^ "\n" | None -> "")
              name page_url in
            [ div ~a:[ a_class [ "error-box" ] ]
                ([ p [ span ~a:[ a_class [ "fail" ] ] [ txt "✗ Build failed" ];
                       txt (Printf.sprintf " — %s." (build_fail_phrase bb.category)) ];
                   p [ txt "Blessed build job: ";
                       job_link bb.build_hash "view build log" ] ]
                 @ (match bb.error with
                    | Some e -> [ p [ code [ txt e ] ] ] | None -> [])
                 @ [ report_affordance ~repo:"ocurrent/ocaml-docs-ci"
                       ~title:(Printf.sprintf "Build failure: %s" pkg_str)
                       ~body
                       ~lead:(Printf.sprintf
                         "If you believe %s should compile correctly on %s, \
                          please comment on the ocurrent/ocaml-docs-ci issues:"
                         pkg_str os_label) ]) ]
          | _, Some bd when bd.status <> "success" ->
            let univ = if bd.universe = "" then "(unknown)" else bd.universe in
            let body = Printf.sprintf
              "Documentation generation failed for %s, although the package \
               built successfully.\n\nUniverse: %s\nDoc job hash: %s\n\
               Profile: %s\nPage: %s\n"
              pkg_str univ bd.build_hash name page_url in
            [ div ~a:[ a_class [ "warn" ] ]
                [ p [ span ~a:[ a_class [ "fail" ] ] [ txt "⚠ Docs failed" ];
                      txt " — the package built, but odoc failed to \
                           generate documentation." ];
                  p ([ txt "Blessed docs job: ";
                       job_link bd.build_hash "view docs log" ]
                     @ (if bd.universe <> "" then
                          [ txt " · universe: ";
                            a ~a:[ a_href (Printf.sprintf
                                             "/profiles/%s/u/%s" name bd.universe) ]
                              [ Templates.sha_span bd.universe ] ]
                        else []));
                  report_affordance ~repo:"ocaml/odoc"
                    ~title:(Printf.sprintf "Doc generation failure: %s" pkg_str)
                    ~body
                    ~lead:(Printf.sprintf
                      "If you believe %s's documentation should build, please \
                       comment on the ocaml/odoc issues:" pkg_str) ] ]
          | Some _, _ ->
            [ p [ em [ txt "No rendered docs found on disk, though the latest \
                            blessed build and docs succeeded — the output may \
                            still be syncing." ] ] ]
          | None, _ ->
            (match latest_cascade_dep () with
             | Some dep -> cascade_blurb dep
             | None ->
               [ p [ em [ txt "Not built in the latest run: a build dependency \
                               may have failed, or this version isn't the \
                               blessed one. See the history below." ] ] ])
      in
      (* Build-deps explorer: per-build folded dep lists plus a
         client-side "compare selected" diff (version changes /
         present-in-only-some). Data is the build_deps.jsonl side-log;
         doc nodes have none, and older data has none until a run with
         the side-log writer lands — in which case this renders nothing. *)
      (* Compare control, shown above the History table. The per-build
         dep lists + checkboxes live inline in the table's "Deps" column;
         this just wires the selected builds into a version diff. Only
         worth showing when ≥2 builds have recorded deps. *)
      let compare_control =
        if Hashtbl.length build_deps_map < 2 then []
        else begin
          let data_json =
            Yojson.Safe.to_string
              (`Assoc (Hashtbl.fold (fun k v acc ->
                 (k, `List (List.map (fun d -> `String d) v)) :: acc)
                 build_deps_map [])) in
          let js_logic = {script|
(function(){
  function split(s){var i=s.lastIndexOf('.');return i<0?[s,'']:[s.slice(0,i),s.slice(i+1)];}
  function esc(s){return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
  function run(){
    var sel=[].slice.call(document.querySelectorAll('.bd-sel:checked')).map(function(c){return c.value;});
    var out=document.getElementById('bd-cmp-result');
    if(sel.length<2){out.innerHTML='<em>Tick at least two build rows below, then Compare.</em>';return;}
    var maps=sel.map(function(h){var m={};(BD_DEPS[h]||[]).forEach(function(d){var p=split(d);m[p[0]]=p[1];});return m;});
    var names={};maps.forEach(function(m){for(var k in m)names[k]=1;});
    var rows='';
    Object.keys(names).sort().forEach(function(n){
      var vs=maps.map(function(m){return m.hasOwnProperty(n)?m[n]:null;});
      if(vs.every(function(v){return v===vs[0];}))return;
      var cells=vs.map(function(v){return '<td>'+(v===null?'<span style="color:#999">absent</span>':esc(v))+'</td>';}).join('');
      rows+='<tr><td>'+esc(n)+'</td>'+cells+'</tr>';
    });
    var hdr=sel.map(function(h){return '<th>'+esc(h.slice(0,12))+'</th>';}).join('');
    out.innerHTML = rows
      ? '<table class="data"><thead><tr><th>Dependency</th>'+hdr+'</tr></thead><tbody>'+rows+'</tbody></table>'
      : '<em>No dependency differences among the selected builds.</em>';
  }
  var btn=document.getElementById('bd-compare-btn');
  if(btn)btn.addEventListener('click',run);
})();
|script} in
          [ Unsafe.data (Printf.sprintf
              "<p class=\"crumbs\">Tick two or more build rows' checkboxes \
               (Deps column), then <button type=\"button\" \
               id=\"bd-compare-btn\">compare deps</button> for version \
               differences.</p><div id=\"bd-cmp-result\"></div>\
               <script>var BD_DEPS=%s;\n%s</script>"
              data_json js_logic) ]
        end
      in
      (* Solver failure: if this target never produced a dependency
         solution, there's no build/history to show — surface the
         solver's explanation instead. Shown above the build/doc
         diagnostic since it's the upstream reason nothing was built. *)
      let solver_failure_block =
        match List.find_map (fun snap -> read_solver_failure snap pkg_str)
                snaps with
        | None -> []
        | Some err ->
          [ h3 [ txt "Solver failure" ];
            p [ span ~a:[ a_class [ "fail" ] ] [ txt "⚠ Solve failed" ];
                txt " — no dependency solution was found for this \
                     package version, so it was never built. The \
                     solver's explanation:" ];
            pre [ txt err ] ]
      in
      Context.respond_ok web_ctx ([
        Templates.style_block; crumbs;
        h2 [ txt pkg_str ] ]
        @ solver_failure_block
        @ status_block
        @ [ h3 [ txt "History" ] ]
        @ compare_control
        @ history_block)
  end

(* ── /profiles/<name>/u/<hash> ────────────────────────────────── *)

(** The package versions making up a universe (a doc-dep closure).
    The manifest is written per-snapshot, but a universe hash is
    content-addressed, so any snapshot recording it gives the same
    set — we take the first match scanning newest-first. *)
let universe_page ~ctx name hash =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let snaps = list_snapshots_newest_first ctx name in
      let manifest = List.find_map (fun snap ->
        Day11_lib.Universe_manifest.read_manifest ~snapshot_dir:snap ~hash)
        snaps
      in
      let crumbs = Templates.breadcrumbs [
        Some "/profiles", "Profiles";
        Some ("/profiles/" ^ name), name;
        None, "universe";
      ] in
      let body = match manifest with
        | None ->
          [ p [ em [ txt "No manifest found for this universe in any \
                          snapshot. It may pre-date universe metadata, \
                          or the snapshots have been pruned." ] ] ]
        | Some m ->
          let row pv =
            let cell = match String.index_opt pv '.' with
              | None -> td [ txt pv ]
              | Some i ->
                let n = String.sub pv 0 i in
                let v = String.sub pv (i + 1) (String.length pv - i - 1) in
                td [ a ~a:[ a_href (Printf.sprintf
                                      "/profiles/%s/p/%s/%s" name n v) ]
                       [ txt pv ] ]
            in
            tr [ cell ]
          in
          [ p [ txt (Printf.sprintf "%d packages in this universe."
                       (List.length m.packages)) ];
            table ~a:[ a_class [ "data" ] ]
              ~thead:(thead [ tr [ th [ txt "Package" ] ] ])
              (List.map row m.packages) ]
      in
      Context.respond_ok web_ctx
        ([ Templates.style_block; crumbs;
           h2 [ txt "Universe "; Templates.sha_span hash ] ] @ body)
  end

(* ── /profiles/<name>/builds/<hash>/log ──────────────────────── *)

(** Read [layer.log] for a build hash and serve it as text/plain.
    The hash points into the per-arch cache dir (derived from the
    profile's [os_dir_name]). Layer dirs use the first 12 chars of
    the full hash as the directory name; we accept either. Used as
    the link target from the [build_hash] cell of the package
    history table — the answer to "why did this build fail?". *)
let build_log_view ~ctx name hash =
  object
    inherit Resource.t
    val! can_get = `Viewer
    method! private get web_ctx =
      let open Lwt.Syntax in
      let* response =
        match Profile.load ~dir:ctx.profile_dir ~name with
        | Error (`Msg e) ->
          Context.respond_error web_ctx
            `Not_found (Printf.sprintf "no such profile: %s (%s)" name e)
        | Ok profile ->
          let os_dir = Profile.os_dir_name profile in
          let short = if String.length hash <= 12 then hash
                      else String.sub hash 0 12 in
          let log_path = Fpath.(ctx.cache_dir / os_dir / short / "layer.log") in
          (match Bos.OS.File.read log_path with
           | Error _ ->
             Context.respond_error web_ctx
               `Not_found (Printf.sprintf
                  "no log for build %s (looked at %s)"
                  short (Fpath.to_string log_path))
           | Ok body ->
             let headers = Cohttp.Header.init_with "Content-Type"
               "text/plain; charset=utf-8" in
             Cohttp_lwt_unix.Server.respond_string
               ~headers ~status:`OK ~body ())
      in
      Lwt.return response
  end
