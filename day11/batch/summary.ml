type build_outcome = {
  pkg : OpamPackage.t;
  build_hash : string;
  success : bool;
  log_file : Fpath.t option;
  blessed : bool;
}

type doc_outcome = {
  pkg : OpamPackage.t;
  success : bool;
  layer_hash : string;
  log_file : Fpath.t option;
  blessed : bool;
}

type results = {
  builds : build_outcome list;
  docs : doc_outcome list;
  targets : OpamPackage.t list;
}

let classify_log log_file =
  match log_file with
  | None -> ("build_failure", None)
  | Some path ->
    match Bos.OS.File.read path with
    | Error _ -> ("build_failure", None)
    | Ok content ->
      let (_status, category, error) =
        Day11_lib.Classify.classify_build_log content
      in
      (category, error)

(* History writes happen incrementally inside {!Recorder} now. *)

(* Aggregate the run's per-node build/doc outcomes into [status.json].
   Cache hits are represented as successful outcomes by the executor, so
   this reflects the full plan state — not just freshly-dispatched nodes.
   The daemon pipeline writes [status.json] the same way, from its own
   collapsed build results (see {!Docs_ci_pipelines.Docs}). *)
let write_status ~snapshot_dir ~run_id (results : results) =
  (* The CLI outcomes only carry a success bool, so a cascade can't be
     told apart from a real failure here ([cascaded = false]); the daemon
     pipeline, which has the DAG, does make that distinction. Keyed by
     package so we can drive both status.json and final_status.json. *)
  let pkg_outcomes =
    List.map (fun (b : build_outcome) ->
      (OpamPackage.to_string b.pkg,
       { Day11_lib.Status_index.is_doc = false;
         blessed = b.blessed; ok = b.success; cascaded = false }))
      results.builds
    @ List.map (fun (d : doc_outcome) ->
      (OpamPackage.to_string d.pkg,
       { Day11_lib.Status_index.is_doc = true;
         blessed = d.blessed; ok = d.success; cascaded = false }))
      results.docs
  in
  let status =
    Day11_lib.Status_index.of_outcomes ~run_id
      ~scanned:(List.length results.targets)
      (List.map snd pkg_outcomes)
  in
  Day11_lib.Status_index.write ~dir:snapshot_dir status;
  Day11_lib.Status_index.write_final_status ~dir:snapshot_dir
    (Day11_lib.Status_index.final_status_of_outcomes pkg_outcomes)

let finish ~snapshot_dir ~packages_dir:_ ~run_info results =
  let run_id = Day11_lib.Run_log.get_id run_info in
  (* History is written incrementally by [Recorder] now. *)
  write_status ~snapshot_dir ~run_id results;
  let builds_ok =
    List.length (List.filter (fun (b : build_outcome) -> b.success) results.builds)
  in
  let builds_fail = List.length results.builds - builds_ok in
  let docs_ok =
    List.length (List.filter (fun (d : doc_outcome) -> d.success) results.docs)
  in
  let failures =
    List.filter_map (fun (b : build_outcome) ->
      if b.success then None
      else
        let cat, _ = classify_log b.log_file in
        Some (OpamPackage.to_string b.pkg, cat)
    ) results.builds
  in
  Printf.printf "Build: %d success, %d failed\n" builds_ok builds_fail;
  Printf.printf "Docs:  %d generated\n" docs_ok;
  if failures <> [] then begin
    Printf.printf "Failures:\n";
    List.iter (fun (p, cat) ->
      Printf.printf "  %s (%s)\n" p cat
    ) failures
  end;
  Day11_lib.Run_log.finish_run run_info
    ~targets_requested:(List.length results.targets)
    ~packages_built:builds_ok
    ~packages_failed:builds_fail
    ~docs_generated:docs_ok
    ~failures
