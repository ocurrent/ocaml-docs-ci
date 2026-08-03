(** Low-level layer cache inspection tool.

    Uses only the day11_layer library — no opam, no solver, no build pipeline.
    Reads layer.json files and lists directory contents directly, without
    interpreting any domain-specific sidecar (build.json, doc.json, etc.).

    For domain-aware inspection (package names, phase classification) use a
    higher-level tool that depends on the appropriate domain library. *)

open Cmdliner
module L = Day11_layer

(* ── Helpers ─────────────────────────────────────────────────────── *)

let fpath = Fpath.v

(** Status string derived from layer meta. *)
let status_of (m : L.Meta.t) =
  if m.exit_status = 0 then "ok"
  else if m.failed_dep <> None then "cascade"
  else "fail"

(** Short hash for display. *)
let short h = if String.length h >= 12 then String.sub h 0 12 else h

let load_meta env layer_dir = L.Meta.load env Fpath.(layer_dir / "layer.json")
(* layer_dir comes from fold_layers which walks the filesystem
     directly — it doesn't have a hash, so we can't use Layer.t here *)

(** List every non-layer-core file in the layer directory. Used to show what
    sidecars etc. are present. *)
let list_extras env layer_dir =
  let core = [ "layer.json"; "layer.log"; "last_used"; "fs" ] in
  try
    Eio.Path.read_dir Eio.Path.(env#fs / Fpath.to_string layer_dir)
    |> List.filter (fun f -> not (List.mem f core))
    |> List.sort compare
  with _ -> []

(** Walk all layer directories under [os_dir] and apply [f]. *)
let fold_layers env os_dir init f =
  L.Scan.list_layers env os_dir
  |> List.fold_left
       (fun acc (name, layer_dir) ->
         match load_meta env layer_dir with
         | Ok meta -> f acc name layer_dir meta
         | Error _ -> acc)
       init

(** Check whether a named file exists in a layer dir. *)
let has_file env layer_dir name =
  Eio.Path.is_file Eio.Path.(env#fs / Fpath.to_string Fpath.(layer_dir / name))

(* ── list ────────────────────────────────────────────────────────── *)

let cmd_list env os_dir status has limit sort_lru =
  let os_dir = fpath os_dir in
  let matches _name layer_dir (m : L.Meta.t) =
    (match status with None -> true | Some s -> status_of m = s)
    &&
    match has with
    | [] -> true
    | files -> List.for_all (has_file env layer_dir) files
  in
  let entries =
    fold_layers env os_dir [] (fun acc name layer_dir m ->
        if matches name layer_dir m then
          (name, layer_dir, m, L.Last_used.get env layer_dir) :: acc
        else acc)
  in
  let entries =
    if sort_lru then
      List.sort
        (fun (_, _, _, a) (_, _, _, b) ->
          let av = match a with Some t -> t | None -> 0.0 in
          let bv = match b with Some t -> t | None -> 0.0 in
          compare av bv)
        entries
    else
      List.sort
        (fun (_, _, a, _) (_, _, b, _) ->
          String.compare a.L.Meta.created_at b.L.Meta.created_at)
        entries
  in
  let n_total = List.length entries in
  let entries =
    match limit with
    | Some n when n > 0 ->
        let rec take k = function
          | [] -> []
          | _ when k = 0 -> []
          | x :: xs -> x :: take (k - 1) xs
        in
        take n entries
    | _ -> entries
  in
  let fmt_lru = function
    | None -> "(never)"
    | Some t ->
        let tm = Unix.gmtime t in
        Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d" (tm.tm_year + 1900)
          (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  in
  let date_col_label = if sort_lru then "LAST USED" else "CREATED" in
  Printf.printf "%-14s  %-8s  %-19s  %-8s  %s\n" "HASH" "STATUS" date_col_label
    "DISK" "EXTRA FILES";
  Printf.printf "%-14s  %-8s  %-19s  %-8s  %s\n" (String.make 12 '-')
    (String.make 8 '-') (String.make 19 '-') (String.make 8 '-')
    (String.make 30 '-');
  List.iter
    (fun (name, layer_dir, (m : L.Meta.t), lru) ->
      let hash =
        match String.index_opt name '-' with
        | Some i -> String.sub name (i + 1) (String.length name - i - 1)
        | None -> name
      in
      let date_col =
        if sort_lru then fmt_lru lru
        else if String.length m.created_at >= 19 then
          String.sub m.created_at 0 19
        else m.created_at
      in
      let extras = list_extras env layer_dir in
      let extras_s =
        if extras = [] then "(none)" else String.concat " " extras
      in
      let disk_s =
        if m.disk_usage >= 1_000_000 then
          Printf.sprintf "%dM" (m.disk_usage / 1_000_000)
        else if m.disk_usage >= 1_000 then
          Printf.sprintf "%dK" (m.disk_usage / 1_000)
        else Printf.sprintf "%dB" m.disk_usage
      in
      Printf.printf "%-14s  %-8s  %-19s  %-8s  %s\n" (short hash) (status_of m)
        date_col disk_s extras_s)
    entries;
  Printf.printf "\n(showing %d of %d layers)\n" (List.length entries) n_total;
  0

(* ── show ────────────────────────────────────────────────────────── *)

let find_layer_by_prefix env os_dir prefix =
  L.Scan.list_layers env os_dir
  |> List.filter_map (fun (name, layer_dir) ->
      let h =
        match String.index_opt name '-' with
        | Some i -> String.sub name (i + 1) (String.length name - i - 1)
        | None -> name
      in
      if
        String.length h >= String.length prefix
        && String.sub h 0 (String.length prefix) = prefix
      then Some (name, layer_dir)
      else None)

let with_resolved_layer env os_dir hash_prefix f =
  let os_dir = fpath os_dir in
  match find_layer_by_prefix env os_dir hash_prefix with
  | [] ->
      Printf.eprintf "No layer with hash prefix %s\n" hash_prefix;
      1
  | _ :: _ :: _ as matches ->
      Printf.eprintf "Ambiguous prefix %s, matches:\n" hash_prefix;
      List.iter (fun (n, _) -> Printf.eprintf "  %s\n" n) matches;
      2
  | [ (name, layer_dir) ] -> f os_dir name layer_dir

let cmd_show env os_dir hash_prefix =
  with_resolved_layer env os_dir hash_prefix @@ fun _os_dir name layer_dir ->
  match load_meta env layer_dir with
  | Error (`Msg e) ->
      Printf.eprintf "%s: %s\n" name e;
      1
  | Ok m ->
      Printf.printf "Layer:    %s\n" name;
      Printf.printf "Path:     %s\n" (Fpath.to_string layer_dir);
      Printf.printf "Status:   %s (exit %d)\n" (status_of m) m.exit_status;
      Printf.printf "Created:  %s\n" m.created_at;
      (match L.Last_used.get env layer_dir with
      | Some t ->
          let tm = Unix.gmtime t in
          Printf.printf "Last used: %04d-%02d-%02dT%02d:%02d:%02dZ\n"
            (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min
            tm.tm_sec
      | None -> Printf.printf "Last used: (never recorded)\n");
      Printf.printf "Base:     %s\n" m.base_hash;
      Printf.printf "UID/GID:  %d:%d\n" m.uid m.gid;
      Printf.printf "Disk:     %d bytes\n" m.disk_usage;
      (match m.failed_dep with
      | Some d -> Printf.printf "Failed dep: %s\n" d
      | None -> ());
      Printf.printf "\nParent layers (%d):\n" (List.length m.parent_hashes);
      List.iter (fun h -> Printf.printf "  %s\n" (short h)) m.parent_hashes;
      let extras = list_extras env layer_dir in
      List.iter
        (fun f ->
          let p = Fpath.(layer_dir / f) in
          let p_ep = Eio.Path.(env#fs / Fpath.to_string p) in
          Printf.printf "\nSidecar: %s\n" f;
          match
            try Ok (Yojson.Safe.from_string (Eio.Path.load p_ep))
            with exn -> Error exn
          with
          | Ok json ->
              let pretty = Yojson.Safe.pretty_to_string json in
              (* Indent each line by two spaces for visual nesting. *)
              let lines = String.split_on_char '\n' pretty in
              List.iter (fun line -> Printf.printf "  %s\n" line) lines
          | Error exn ->
              let size =
                try Optint.Int63.to_int (Eio.Path.stat ~follow:true p_ep).size
                with _ -> -1
              in
              Printf.printf "  (not valid JSON: %s, %d bytes)\n"
                (Printexc.to_string exn) size)
        extras;
      if m.timing <> [] then (
        Printf.printf "\nTiming (s):\n";
        List.iter
          (fun (tname, secs) ->
            Printf.printf "  %-16s %.3f\n" (tname ^ ":") secs)
          m.timing);
      0

(* ── tree ────────────────────────────────────────────────────────── *)

let cmd_tree env os_dir hash_prefix =
  with_resolved_layer env os_dir hash_prefix @@ fun os_dir name _layer_dir ->
  let full_hash =
    match String.index_opt name '-' with
    | Some i -> String.sub name (i + 1) (String.length name - i - 1)
    | None -> name
  in
  let visited = Hashtbl.create 32 in
  let rec walk depth dname =
    let prefix = String.make (depth * 2) ' ' in
    let layer_dir = Fpath.(os_dir / dname) in
    if Hashtbl.mem visited dname then
      Printf.printf "%s%s  (already shown)\n" prefix dname
    else (
      Hashtbl.add visited dname ();
      match load_meta env layer_dir with
      | Error _ -> Printf.printf "%s%s  (no layer.json)\n" prefix dname
      | Ok m ->
          let extras = list_extras env layer_dir in
          let tag =
            if extras = [] then "" else " [" ^ String.concat " " extras ^ "]"
          in
          Printf.printf "%s%s  %s%s\n" prefix dname (status_of m) tag;
          List.iter
            (fun h ->
              let dep_name = "build-" ^ short h in
              walk (depth + 1) dep_name)
            m.parent_hashes)
  in
  walk 0 ("build-" ^ short full_hash);
  0

(* ── stats ───────────────────────────────────────────────────────── *)

let cmd_stats env os_dir =
  let os_dir = fpath os_dir in
  let by_status = Hashtbl.create 4 in
  let by_extra_set = Hashtbl.create 8 in
  let total_disk = ref 0 in
  let n =
    fold_layers env os_dir 0 (fun acc _ layer_dir m ->
        let st = status_of m in
        let cur = try Hashtbl.find by_status st with Not_found -> 0 in
        Hashtbl.replace by_status st (cur + 1);
        let key = String.concat " " (list_extras env layer_dir) in
        let key = if key = "" then "(none)" else key in
        let cur2 = try Hashtbl.find by_extra_set key with Not_found -> 0 in
        Hashtbl.replace by_extra_set key (cur2 + 1);
        total_disk := !total_disk + m.disk_usage;
        acc + 1)
  in
  Printf.printf "Total layers: %d\n" n;
  Printf.printf "Total disk:   %d bytes (%.1f GB)\n" !total_disk
    (float_of_int !total_disk /. 1e9);
  Printf.printf "\nBy status:\n";
  Hashtbl.iter (fun k v -> Printf.printf "  %-10s  %d\n" k v) by_status;
  Printf.printf "\nBy extra-file set (i.e. which sidecars are present):\n";
  let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) by_extra_set [] in
  let entries = List.sort (fun (_, a) (_, b) -> compare b a) entries in
  List.iter
    (fun (key, count) -> Printf.printf "  %-40s  %d\n" key count)
    entries;
  0

(* ── log ─────────────────────────────────────────────────────────── *)

let cmd_log env os_dir hash_prefix =
  with_resolved_layer env os_dir hash_prefix @@ fun os_dir name _layer_dir ->
  let layer = L.Layer.of_hash ~os_dir name in
  let lp = L.Layer.log_path layer in
  match
    try Ok (Eio.Path.load Eio.Path.(env#fs / Fpath.to_string lp))
    with exn -> Error (Printexc.to_string exn)
  with
  | Ok content ->
      print_string content;
      0
  | Error e ->
      Printf.eprintf "%s\n" e;
      1

(* ── CLI wiring ──────────────────────────────────────────────────── *)

let os_dir_term =
  let doc =
    "Path to the OS-specific cache directory containing build-* subdirectories"
  in
  Arg.(required & opt (some string) None & info [ "os-dir" ] ~docv:"DIR" ~doc)

let status_term =
  let doc = "Filter by status (ok, fail, cascade)" in
  Arg.(
    value & opt (some string) None & info [ "status"; "s" ] ~docv:"STATUS" ~doc)

let has_term =
  let doc =
    "Filter to layers that have a file of this name in their directory. \
     Repeatable — all specified files must be present. Use e.g. [--has \
     build.json] to find opam build layers, [--has doc.json] for doc layers."
  in
  Arg.(value & opt_all string [] & info [ "has" ] ~docv:"FILE" ~doc)

let limit_term =
  let doc = "Limit number of results" in
  Arg.(value & opt (some int) None & info [ "limit"; "n" ] ~docv:"N" ~doc)

let sort_lru_term =
  let doc = "Sort by last-used time, oldest first" in
  Arg.(value & flag & info [ "sort-by-lru"; "lru" ] ~doc)

let hash_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"HASH" ~doc:"Layer hash (or prefix)")

let build_main env =
  let list_cmd =
    let info = Cmd.info "list" ~doc:"List layers in the cache" in
    Cmd.v info
      Term.(
        const (cmd_list env)
        $ os_dir_term
        $ status_term
        $ has_term
        $ limit_term
        $ sort_lru_term)
  in
  let show_cmd =
    let info =
      Cmd.info "show" ~doc:"Show layer.json contents and list of extra files"
    in
    Cmd.v info Term.(const (cmd_show env) $ os_dir_term $ hash_term)
  in
  let tree_cmd =
    let info =
      Cmd.info "tree" ~doc:"Show parent tree of a layer (follows parent_hashes)"
    in
    Cmd.v info Term.(const (cmd_tree env) $ os_dir_term $ hash_term)
  in
  let stats_cmd =
    let info =
      Cmd.info "stats"
        ~doc:
          "Summary: total count, disk usage, breakdown by status and \
           extra-file set"
    in
    Cmd.v info Term.(const (cmd_stats env) $ os_dir_term)
  in
  let log_cmd =
    let info = Cmd.info "log" ~doc:"Print the layer's build log (layer.log)" in
    Cmd.v info Term.(const (cmd_log env) $ os_dir_term $ hash_term)
  in
  let info =
    Cmd.info "day11-layer-cli"
      ~doc:
        "Low-level inspection of the day11 layer cache. Strictly generic: \
         knows about layer.json and the filesystem layout, but treats \
         domain-specific sidecar files as opaque JSON blobs (which it \
         pretty-prints in 'show')."
      ~version:"0.3"
  in
  Cmd.group info [ list_cmd; show_cmd; tree_cmd; stats_cmd; log_cmd ]

let () =
  Eio_main.run @@ fun env ->
  let env = (env :> Eio_unix.Stdenv.base) in
  exit (Cmd.eval' (build_main env))
