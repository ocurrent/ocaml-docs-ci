type t = { id : string; start_time : float; run_dir : string }

type summary = {
  run_id : string;
  start_time : float;
  end_time : float;
  duration : float;
  targets_requested : int;
  packages_built : int;
  packages_failed : int;
  docs_generated : int;
  failures : (string * string) list;
}

let log_base_dir = ref "/var/log/day11"
let set_log_base_dir dir = log_base_dir := dir

let generate_run_id () =
  let t = Unix.gettimeofday () in
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02d-%02d%02d%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let format_time t =
  let tm = Unix.localtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let get_id (t : t) = t.id
let get_run_dir (t : t) = t.run_dir
let get_start_time (t : t) = t.start_time

let mkdir_p path =
  let rec create dir =
    if not (Sys.file_exists dir) then (
      create (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  create path

let start_run () =
  let id = generate_run_id () in
  let runs_dir = Filename.concat !log_base_dir "runs" in
  let run_dir = Filename.concat runs_dir id in
  mkdir_p run_dir;
  mkdir_p (Filename.concat run_dir "build");
  mkdir_p (Filename.concat run_dir "docs");
  { id; start_time = Unix.gettimeofday (); run_dir }

let update_latest_symlink run_info =
  let latest = Filename.concat !log_base_dir "latest" in
  let target = Filename.concat "runs" run_info.id in
  (try Unix.unlink latest with Unix.Unix_error _ -> ());
  try Unix.symlink target latest
  with Unix.Unix_error (err, _, _) ->
    Printf.eprintf "[run_log] Warning: failed to create latest symlink: %s\n%!"
      (Unix.error_message err)

let add_build_log run_info ~package ~source_log =
  let dest =
    Filename.concat run_info.run_dir
      (Filename.concat "build" (package ^ ".log"))
  in
  if Sys.file_exists source_log then
    try Unix.symlink source_log dest
    with Unix.Unix_error _ -> (
      try
        let content =
          In_channel.with_open_text source_log In_channel.input_all
        in
        Out_channel.with_open_text dest (fun oc ->
            Out_channel.output_string oc content)
      with _ -> ())

let add_doc_log run_info ~package ~source_log ~layer_hash =
  let filename =
    if layer_hash = "" then package ^ ".log"
    else package ^ "." ^ layer_hash ^ ".log"
  in
  let dest =
    Filename.concat run_info.run_dir (Filename.concat "docs" filename)
  in
  if Sys.file_exists source_log then (
    (try Unix.unlink dest with Unix.Unix_error _ -> ());
    try Unix.symlink source_log dest
    with Unix.Unix_error _ -> (
      try
        let content =
          In_channel.with_open_text source_log In_channel.input_all
        in
        Out_channel.with_open_text dest (fun oc ->
            Out_channel.output_string oc content)
      with _ -> ()))

(* --- Incremental phase files --- *)

let write_phase_json run_info name json =
  let path = Filename.concat run_info.run_dir (name ^ ".json") in
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc (Yojson.Safe.pretty_to_string json))

let write_plan run_info ~repos_with_shas ~n_targets ~ocaml_version ~with_doc
    ~all_versions ~small_universe =
  write_phase_json run_info "plan"
    (`Assoc
       [
         ("timestamp", `String (format_time (Unix.gettimeofday ())));
         ( "repos",
           `List
             (List.map
                (fun (repo, sha) ->
                  `Assoc [ ("path", `String repo); ("commit", `String sha) ])
                repos_with_shas) );
         ("targets", `Int n_targets);
         ( "ocaml_version",
           match ocaml_version with Some v -> `String v | None -> `Null );
         ("with_doc", `Bool with_doc);
         ("all_versions", `Bool all_versions);
         ("small_universe", `Bool small_universe);
       ])

let write_solve run_info ~n_solved ~n_failed =
  write_phase_json run_info "solve"
    (`Assoc
       [
         ("timestamp", `String (format_time (Unix.gettimeofday ())));
         ("solved", `Int n_solved);
         ("failed", `Int n_failed);
       ])

let write_dag run_info ~n_build ~n_cached ~n_need_build =
  write_phase_json run_info "dag"
    (`Assoc
       [
         ("timestamp", `String (format_time (Unix.gettimeofday ())));
         ("build_nodes", `Int n_build);
         ("cached", `Int n_cached);
         ("need_build", `Int n_need_build);
       ])

let write_doc_dag run_info ~n_build ~n_tool ~n_doc_all ~n_compile ~n_link =
  write_phase_json run_info "doc_dag"
    (`Assoc
       [
         ("timestamp", `String (format_time (Unix.gettimeofday ())));
         ("build_nodes", `Int n_build);
         ("tool_nodes", `Int n_tool);
         ("doc_all_nodes", `Int n_doc_all);
         ("compile_nodes", `Int n_compile);
         ("link_nodes", `Int n_link);
         ("total", `Int (n_build + n_tool + n_doc_all + n_compile + n_link));
       ])

let build_log_lock = Mutex.create ()
let build_log_oc : out_channel option ref = ref None

let log_build_result run_info ~pkg ~hash ~status ~failed_dep ?(kind = "build")
    ?(layer_dir = "") () =
  Mutex.lock build_log_lock;
  let oc =
    match !build_log_oc with
    | Some oc -> oc
    | None ->
        let path = Filename.concat run_info.run_dir "build.jsonl" in
        let oc =
          open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 path
        in
        build_log_oc := Some oc;
        oc
  in
  let json =
    `Assoc
      ([
         ("t", `String (format_time (Unix.gettimeofday ())));
         ("kind", `String kind);
         ("pkg", `String pkg);
         ("hash", `String hash);
         ("status", `String status);
       ]
      @ (if layer_dir = "" then [] else [ ("layer_dir", `String layer_dir) ])
      @
      match failed_dep with
      | Some d -> [ ("failed_dep", `String d) ]
      | None -> [])
  in
  output_string oc (Yojson.Safe.to_string json);
  output_char oc '\n';
  flush oc;
  Mutex.unlock build_log_lock

let close_build_log () =
  Mutex.lock build_log_lock;
  (match !build_log_oc with
  | Some oc ->
      close_out_noerr oc;
      build_log_oc := None
  | None -> ());
  Mutex.unlock build_log_lock

let write_dag_structure run_info (nodes : Day11_opam_layer.Build.t list) =
  let path = Filename.concat run_info.run_dir "dag_structure.jsonl" in
  Out_channel.with_open_text path (fun oc ->
      List.iter
        (fun (node : Day11_opam_layer.Build.t) ->
          let json =
            `Assoc
              [
                ("hash", `String node.hash);
                ("pkg", `String (OpamPackage.to_string node.pkg));
                ( "deps",
                  `List
                    (List.map
                       (fun (dep : Day11_opam_layer.Build.t) ->
                         `String dep.hash)
                       node.deps) );
              ]
          in
          output_string oc (Yojson.Safe.to_string json);
          output_char oc '\n')
        nodes)

let summary_to_json summary =
  let failures_json =
    `List
      (List.map
         (fun (pkg, err) ->
           `Assoc [ ("package", `String pkg); ("error", `String err) ])
         summary.failures)
  in
  `Assoc
    [
      ("run_id", `String summary.run_id);
      ("start_time", `Float summary.start_time);
      ("end_time", `Float summary.end_time);
      ("duration", `Float summary.duration);
      ("targets_requested", `Int summary.targets_requested);
      ("packages_built", `Int summary.packages_built);
      ("packages_failed", `Int summary.packages_failed);
      ("docs_generated", `Int summary.docs_generated);
      ("failures", failures_json);
    ]

let write_summary run_info summary =
  let path = Filename.concat run_info.run_dir "summary.json" in
  let json = summary_to_json summary in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.with_open_text path (fun oc ->
      Out_channel.output_string oc content)

let finish_run (run_info : t) ~targets_requested ~packages_built
    ~packages_failed ~docs_generated ~failures =
  let finish_time = Unix.gettimeofday () in
  let summary : summary =
    {
      run_id = run_info.id;
      start_time = run_info.start_time;
      end_time = finish_time;
      duration = finish_time -. run_info.start_time;
      targets_requested;
      packages_built;
      packages_failed;
      docs_generated;
      failures;
    }
  in
  write_summary run_info summary;
  update_latest_symlink run_info;
  summary
