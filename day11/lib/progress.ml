type phase = Solving | Blessings | Building | Gc | Completed

type t = {
  run_id : string;
  start_time : string;
  phase : phase;
  targets : string list;
  solutions_found : int;
  solutions_failed : int;
  build_completed : int;
  build_total : int;
  doc_completed : int;
  doc_total : int;
}

let phase_to_string = function
  | Solving -> "solving"
  | Blessings -> "blessings"
  | Building -> "building"
  | Gc -> "gc"
  | Completed -> "completed"

let create ~run_id ~start_time ~targets =
  {
    run_id;
    start_time;
    phase = Solving;
    targets;
    solutions_found = 0;
    solutions_failed = 0;
    build_completed = 0;
    build_total = 0;
    doc_completed = 0;
    doc_total = 0;
  }

let set_phase t phase = { t with phase }

let set_solutions t ~found ~failed =
  { t with solutions_found = found; solutions_failed = failed }

let set_build_total t total = { t with build_total = total; doc_total = total }
let incr_build_completed t = { t with build_completed = t.build_completed + 1 }
let incr_doc_completed t = { t with doc_completed = t.doc_completed + 1 }

let set_completed t ~build ~doc =
  { t with build_completed = build; doc_completed = doc }

let to_json t =
  `Assoc
    [
      ("run_id", `String t.run_id);
      ("start_time", `String t.start_time);
      ("phase", `String (phase_to_string t.phase));
      ("targets", `List (List.map (fun s -> `String s) t.targets));
      ("solutions_found", `Int t.solutions_found);
      ("solutions_failed", `Int t.solutions_failed);
      ("build_completed", `Int t.build_completed);
      ("build_total", `Int t.build_total);
      ("doc_completed", `Int t.doc_completed);
      ("doc_total", `Int t.doc_total);
    ]

let write ~run_dir t =
  let path = Filename.concat run_dir "progress.json" in
  let temp_path = path ^ ".tmp" in
  let json = to_json t in
  let content = Yojson.Safe.pretty_to_string json in
  Out_channel.with_open_text temp_path (fun oc ->
      Out_channel.output_string oc content);
  Unix.rename temp_path path

let delete ~run_dir =
  let path = Filename.concat run_dir "progress.json" in
  try Unix.unlink path with Unix.Unix_error _ -> ()
