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

let generate_status ~snapshot_dir ~packages_dir ~run_id =
  let previous = Day11_lib.Status_index.read ~dir:snapshot_dir in
  let status =
    Day11_lib.Status_index.generate ~packages_dir ~run_id ~previous
  in
  Day11_lib.Status_index.write ~dir:snapshot_dir status;
  status

let finish ~snapshot_dir ~packages_dir ~run_info results =
  let run_id = Day11_lib.Run_log.get_id run_info in
  (* History is written incrementally by [Recorder] now. *)
  ignore (generate_status ~snapshot_dir ~packages_dir ~run_id : Day11_lib.Status_index.t);
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
