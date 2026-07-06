let src = Logs.Src.create "day11.status_index"
    ~doc:"Status index (status.json) read/write"
module Log = (val Logs.src_log src)

type change = {
  package : string;
  build_hash : string;
  blessed : bool;
  from_status : string;
  to_status : string;
}

type t = {
  generated : string;
  run_id : string;
  scanned : int;
  blessed_totals : (string * int) list;
  non_blessed_totals : (string * int) list;
  changes : change list;
  new_packages : string list;
}

let totals_to_json (t : (string * int) list) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, v) -> (k, `Int v)) t)

let totals_of_json (json : Yojson.Safe.t) : (string * int) list =
  match json with
  | `Assoc assoc ->
    List.filter_map (fun (k, v) ->
      match v with
      | `Int n -> Some (k, n)
      | _ -> None
    ) assoc
  | _ -> []

let change_to_json (c : change) : Yojson.Safe.t =
  `Assoc [
    ("package", `String c.package);
    ("build_hash", `String c.build_hash);
    ("blessed", `Bool c.blessed);
    ("from", `String c.from_status);
    ("to", `String c.to_status);
  ]

let change_of_json (json : Yojson.Safe.t) : change option =
  match json with
  | `Assoc assoc ->
    let s key =
      match List.assoc_opt key assoc with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let b key =
      match List.assoc_opt key assoc with
      | Some (`Bool b) -> Some b
      | _ -> None
    in
    (match s "package", s "build_hash", b "blessed", s "from", s "to" with
     | Some package, Some build_hash, Some blessed, Some from_status, Some to_status ->
       Some { package; build_hash; blessed; from_status; to_status }
     | _ -> None)
  | _ -> None

let to_json (t : t) : Yojson.Safe.t =
  `Assoc [
    ("generated", `String t.generated);
    ("run_id", `String t.run_id);
    ("scanned", `Int t.scanned);
    ("blessed_totals", totals_to_json t.blessed_totals);
    ("non_blessed_totals", totals_to_json t.non_blessed_totals);
    ("changes_since_last", `List (List.map change_to_json t.changes));
    ("new_packages", `List (List.map (fun s -> `String s) t.new_packages));
  ]

let of_json (json : Yojson.Safe.t) : t option =
  match json with
  | `Assoc assoc ->
    let s key =
      match List.assoc_opt key assoc with
      | Some (`String s) -> Some s
      | _ -> None
    in
    (match s "generated", s "run_id" with
     | Some generated, Some run_id ->
       let changes =
         match List.assoc_opt "changes_since_last" assoc with
         | Some (`List l) -> List.filter_map change_of_json l
         | _ -> []
       in
       let new_packages =
         match List.assoc_opt "new_packages" assoc with
         | Some (`List l) ->
           List.filter_map (fun j ->
             match j with `String s -> Some s | _ -> None
           ) l
         | _ -> []
       in
       let scanned =
         match List.assoc_opt "scanned" assoc with
         | Some (`Int n) -> n | _ -> 0
       in
       Some {
         generated;
         run_id;
         scanned;
         blessed_totals = totals_of_json
           (match List.assoc_opt "blessed_totals" assoc with
            | Some j -> j | None -> `Assoc []);
         non_blessed_totals = totals_of_json
           (match List.assoc_opt "non_blessed_totals" assoc with
            | Some j -> j | None -> `Assoc []);
         changes;
         new_packages;
       }
     | _ -> None)
  | _ -> None

let iso8601_now () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let incr_totals totals category =
  match List.assoc_opt category totals with
  | Some n -> (category, n + 1) :: List.filter (fun (k, _) -> k <> category) totals
  | None -> (category, 1) :: totals

let list_subdirs dir =
  let dir_s = Fpath.to_string dir in
  if not (Sys.file_exists dir_s) then []
  else
    Sys.readdir dir_s
    |> Array.to_list
    |> List.filter (fun name ->
      let path = Filename.concat dir_s name in
      try Sys.is_directory path with Sys_error _ -> false)

let generate ~packages_dir ~run_id ~previous:_ =
  let pkg_dirs = list_subdirs packages_dir in
  let blessed_totals = ref [] in
  let non_blessed_totals = ref [] in
  let changes = ref [] in
  let new_packages = ref [] in
  let os_dir = Fpath.parent packages_dir in
  let effective_category (e : History.entry) =
    if e.category = "build_failure" && String.length e.build_hash > 0 && e.build_hash <> "none" then
      let layer_json = Fpath.to_string Fpath.(os_dir / e.build_hash / "layer.json") in
      match Yojson.Safe.from_file layer_json with
      | `Assoc assoc ->
        (match List.assoc_opt "exit_status" assoc with
         | Some (`Int (-1)) -> "dependency_failure"
         | _ -> e.category)
      | _ -> e.category
      | exception _ -> e.category
    else
      e.category
  in
  List.iter (fun pkg_str ->
    let latest_entries = History.read_latest ~packages_dir ~pkg_str in
    (* "Live" blessing = most recent blessed=true entry from the
       current run. A build_hash that was blessed in an older run but
       isn't part of the current solution is superseded — it's no
       longer the canonical universe for this package, so we count it
       with the non-blessed siblings rather than as a live-blessed. *)
    let live_blessed = List.find_opt (fun (e : History.entry) ->
      e.blessed && e.run = run_id
    ) latest_entries in
    (match live_blessed with
     | Some e -> blessed_totals := incr_totals !blessed_totals (effective_category e)
     | None -> ());
    List.iter (fun (e : History.entry) ->
      let is_live = match live_blessed with
        | Some lb -> lb.build_hash = e.build_hash
        | None -> false
      in
      if not is_live then
        non_blessed_totals := incr_totals !non_blessed_totals (effective_category e)
    ) latest_entries;
    let all_entries = History.read ~packages_dir ~pkg_str in
    let seen_hashes = Hashtbl.create 16 in
    List.iter (fun (e : History.entry) ->
      if not (Hashtbl.mem seen_hashes e.build_hash) then begin
        Hashtbl.add seen_hashes e.build_hash true;
        if e.run = run_id then begin
          let prev = List.find_opt (fun (e2 : History.entry) ->
            e2.build_hash = e.build_hash && e2.run <> run_id
          ) all_entries in
          match prev with
          | Some prev_entry when (effective_category prev_entry) <> (effective_category e) ->
            changes := {
              package = pkg_str;
              build_hash = e.build_hash;
              blessed = e.blessed;
              from_status = effective_category prev_entry;
              to_status = effective_category e;
            } :: !changes
          | _ -> ()
        end
      end
    ) all_entries;
    let has_old_entries = List.exists (fun (e : History.entry) ->
      e.run <> run_id
    ) all_entries in
    if (not has_old_entries) && all_entries <> [] then
      new_packages := pkg_str :: !new_packages
  ) pkg_dirs;
  let sum totals = List.fold_left (fun acc (_, n) -> acc + n) 0 totals in
  (* App level: always shown regardless of --verbosity, so build
     progress (and whether generate_status runs incrementally or only
     at completion) is visible in the daemon log by default. *)
  Log.app (fun f -> f "generated status (run %s): %d pkg-dirs scanned, \
    blessed=%d non_blessed=%d changes=%d new=%d"
    run_id (List.length pkg_dirs) (sum !blessed_totals)
    (sum !non_blessed_totals) (List.length !changes)
    (List.length !new_packages));
  {
    generated = iso8601_now ();
    run_id;
    scanned = List.length pkg_dirs;
    blessed_totals = !blessed_totals;
    non_blessed_totals = !non_blessed_totals;
    changes = List.rev !changes;
    new_packages = List.rev !new_packages;
  }

let status_path dir = Fpath.to_string Fpath.(dir / "status.json")

let write ~dir t =
  let path = status_path dir in
  let json_str = Yojson.Safe.pretty_to_string (to_json t) in
  let tmp_path = path ^ ".tmp" in
  let oc = open_out tmp_path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc json_str;
    output_char oc '\n');
  Sys.rename tmp_path path

let read ~dir =
  let path = status_path dir in
  if not (Sys.file_exists path) then begin
    (* Normal early in a fresh run: status.json isn't written until the
       first [generate_status] pass completes. At info so "page shows
       no totals" is explainable from the log (file genuinely absent vs.
       a parse failure below). *)
    Log.info (fun f -> f "status.json not present yet: %s" path);
    None
  end
  else
    match Yojson.Safe.from_file path with
    | exception exn ->
      Log.warn (fun f -> f "status.json unreadable / invalid JSON (%s): %s"
        path (Printexc.to_string exn));
      None
    | json ->
      match of_json json with
      | Some _ as r -> r
      | None ->
        Log.warn (fun f -> f "status.json parsed as JSON but failed \
          structural validation (missing or non-string generated/run_id?): \
          %s" path);
        None
