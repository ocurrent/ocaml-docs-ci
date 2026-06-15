module Build = Day11_opam_layer.Build
module Layer = Day11_layer.Layer

type t = {
  env : Eio_unix.Stdenv.base;
  os_dir : Fpath.t;
  packages_dir : Fpath.t;
  blessing_maps : (OpamPackage.t * bool OpamPackage.Map.t) list;
  run_log : Day11_lib.Run_log.t;
  outcomes_lock : Mutex.t;
  outcomes : Summary.build_outcome list ref;
  doc_outcomes : Summary.doc_outcome list ref;
}

let now_iso8601 () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

let create ~env ~os_dir ~packages_dir ~blessing_maps ~run_log =
  { env; os_dir; packages_dir; blessing_maps; run_log;
    outcomes_lock = Mutex.create ();
    outcomes = ref [];
    doc_outcomes = ref [] }

let is_blessed t (node : Build.t) =
  List.exists (fun (_target, map) ->
    Blessing.is_blessed map node.pkg
  ) t.blessing_maps

let log_file_for t (node : Build.t) =
  let dir = Build.dir ~os_dir:t.os_dir node in
  let p = Fpath.(dir / "build.log") in
  if Sys.file_exists (Fpath.to_string p) then Some p else None

let append_outcome t outcome =
  Mutex.lock t.outcomes_lock;
  t.outcomes := outcome :: !(t.outcomes);
  Mutex.unlock t.outcomes_lock

let ensure_symlink t (node : Build.t) =
  let pkg_str = OpamPackage.to_string node.pkg in
  let layer_name = Build.dir_name node in
  ignore (Day11_layer.Symlinks.ensure t.env
    ~packages_dir:t.packages_dir ~id:pkg_str ~layer_name)

let classify_log log_file =
  match log_file with
  | None -> ("build_failure", None)
  | Some path ->
    match Bos.OS.File.read path with
    | Error _ -> ("build_failure", None)
    | Ok content ->
      let (_status, category, error) =
        Day11_lib.Classify.classify_build_log content in
      (category, error)

(** Append one history entry to disk immediately. Per-pkg history.jsonl
    becomes the live event log; downstream regen of [status.json]
    happens on a schedule, not at the end of the OCurrent tick. *)
let append_history t ~node ~status ~category ~error ~blessed ~universe =
  let entry : Day11_lib.History.entry = {
    ts = now_iso8601 ();
    run = Day11_lib.Run_log.get_id t.run_log;
    build_hash = node.Build.hash;
    status;
    category;
    blessed;
    error;
    universe;
  } in
  Day11_lib.History.append ~packages_dir:t.packages_dir
    ~pkg_str:(OpamPackage.to_string node.pkg) entry

let record_build t (node : Build.t) ~success =
  ensure_symlink t node;
  let log_file = log_file_for t node in
  let blessed = is_blessed t node in
  let category, error =
    if success then ("success", None) else classify_log log_file in
  (* Build/tool nodes have no output universe (only doc nodes do). *)
  append_history t ~node
    ~status:(if success then "success" else "failure")
    ~category ~error ~blessed ~universe:"";
  append_outcome t {
    Summary.pkg = node.pkg;
    build_hash = node.hash;
    success;
    log_file;
    blessed;
  };
  let layer_dir =
    Fpath.to_string (Build.dir ~os_dir:t.os_dir node) in
  Day11_lib.Run_log.log_build_result t.run_log
    ~pkg:(OpamPackage.to_string node.pkg)
    ~hash:node.hash
    ~status:(if success then "ok" else "fail")
    ~failed_dep:None
    ~kind:"build" ~layer_dir ()

let record_cascade t ~(failed : Build.t) ~(failed_dep : Build.t) =
  (* Cascades are entirely derivable from [<snapshot_dir>/dag.json] +
     [<os_dir>/layer_status.jsonl] — no on-disk layer state, no
     history entry. We do still note it in the run-log so [day11
     status] reflects per-tick activity, and ensure the package
     symlink so the GUI's package navigation finds the (cascaded)
     package. *)
  ensure_symlink t failed;
  Day11_lib.Run_log.log_build_result t.run_log
    ~pkg:(OpamPackage.to_string failed.pkg)
    ~hash:failed.hash
    ~status:"cascade"
    ~failed_dep:(Some (OpamPackage.to_string failed_dep.pkg))
    ~kind:"build" ()

let append_doc_outcome t outcome =
  Mutex.lock t.outcomes_lock;
  t.doc_outcomes := outcome :: !(t.doc_outcomes);
  Mutex.unlock t.outcomes_lock

let record_doc t (node : Build.t) ~blessed ~universe ~success =
  ensure_symlink t node;
  let log_file = log_file_for t node in
  append_history t ~node
    ~status:(if success then "success" else "failure")
    ~category:(if success then "doc_success" else "doc_failure")
    ~error:None ~blessed ~universe;
  append_doc_outcome t {
    Summary.pkg = node.pkg;
    success;
    layer_hash = node.hash;
    log_file;
    blessed;
  };
  let layer_dir =
    Fpath.to_string (Build.dir ~os_dir:t.os_dir node) in
  Day11_lib.Run_log.log_build_result t.run_log
    ~pkg:(OpamPackage.to_string node.pkg)
    ~hash:node.hash
    ~status:(if success then "ok" else "fail")
    ~failed_dep:None
    ~kind:"doc" ~layer_dir ()

let outcomes t =
  Mutex.lock t.outcomes_lock;
  let r = !(t.outcomes) in
  Mutex.unlock t.outcomes_lock;
  r

let doc_outcomes t =
  Mutex.lock t.outcomes_lock;
  let r = !(t.doc_outcomes) in
  Mutex.unlock t.outcomes_lock;
  r
