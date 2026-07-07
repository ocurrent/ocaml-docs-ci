type report = {
  base : int;
  builds : int;
  docs : int;
  jtw : int;
  solutions : int;
  logs : int;
  packages : int;
  total : int;
}

let dir_size path =
  let path_s = Fpath.to_string path in
  if not (Sys.file_exists path_s) then 0
  else
    (* Use du -sb for accurate byte count *)
    let ic = Unix.open_process_in
      (Printf.sprintf "du -sb %s 2>/dev/null" (Filename.quote path_s)) in
    let result = try Scanf.sscanf (input_line ic) "%d" Fun.id
      with _ -> 0 in
    ignore (Unix.close_process_in ic);
    result

let sum_matching ~dir prefix =
  let dir_s = Fpath.to_string dir in
  if not (Sys.file_exists dir_s) then 0
  else
    Sys.readdir dir_s |> Array.to_list
    |> List.filter (fun name ->
      String.length name >= String.length prefix
      && String.sub name 0 (String.length prefix) = prefix)
    |> List.fold_left (fun acc name ->
      acc + dir_size Fpath.(dir / name)) 0

let is_layer_dir name =
  (* Layer dirs are 12-char hex strings (new format) or build-<hex> (legacy) *)
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  (String.length name = 12 && String.for_all is_hex name)
  || (String.length name > 6
      && String.sub name 0 6 = "build-")

(* Total bytes of all build layers across every os_dir under [cache_dir],
   summed from each layer's recorded [disk_usage] metadata ([layer.json])
   rather than by measuring the tree — so it's a stat+read per layer, not
   a recursive [du]. Still reads one small JSON per layer (hundreds of
   thousands of them at scale), so callers should run it off the event
   loop and infrequently. *)
let layer_meta_total ~cache_dir =
  let cache_s = Fpath.to_string cache_dir in
  let dirs_in d =
    try Sys.readdir d |> Array.to_list with _ -> [] in
  let os_dirs =
    List.filter (fun n ->
      try Sys.is_directory (Filename.concat cache_s n) with _ -> false)
      (dirs_in cache_s)
  in
  List.fold_left (fun acc os ->
    let os_s = Filename.concat cache_s os in
    List.fold_left (fun acc name ->
      if not (is_layer_dir name) then acc
      else
        let lj = Filename.concat (Filename.concat os_s name) "layer.json" in
        match (try Some (Yojson.Safe.from_file lj) with _ -> None) with
        | Some (`Assoc a) ->
          (match List.assoc_opt "disk_usage" a with
           | Some (`Int n) -> acc + n
           | _ -> acc)
        | _ -> acc)
      acc (dirs_in os_s))
    0 os_dirs

let scan ~os_dir ~cache_dir =
  (* The base layer lives under its os_dir now, not in a shared
     [cache_dir/base]. It is not a layer dir (only 12-hex or build-
     prefixed names match [is_layer_dir]), so [builds] below never
     double-counts it. *)
  let base = dir_size Fpath.(os_dir / "base") in
  let builds =
    let dir_s = Fpath.to_string os_dir in
    if not (Sys.file_exists dir_s) then 0
    else
      Sys.readdir dir_s |> Array.to_list
      |> List.filter is_layer_dir
      |> List.fold_left (fun acc name ->
        acc + dir_size Fpath.(os_dir / name)) 0
  in
  let docs = dir_size Fpath.(os_dir / "odoc-store") in
  let jtw = sum_matching ~dir:os_dir "jtw-" in
  let solutions = dir_size Fpath.(cache_dir / "solutions") in
  let logs = dir_size Fpath.(cache_dir / "logs") in
  let packages = dir_size Fpath.(os_dir / "packages") in
  let total = base + builds + docs + jtw + solutions + logs + packages in
  { base; builds; docs; jtw; solutions; logs; packages; total }

let human_size n =
  if n >= 1_073_741_824 then Printf.sprintf "%.1f GB" (float n /. 1_073_741_824.)
  else if n >= 1_048_576 then Printf.sprintf "%.1f MB" (float n /. 1_048_576.)
  else if n >= 1024 then Printf.sprintf "%.1f KB" (float n /. 1024.)
  else Printf.sprintf "%d B" n

let pp fmt r =
  Fmt.pf fmt "@[<v>Base:      %s@,Builds:    %s@,Docs:      %s@,JTW:       %s@,Solutions: %s@,Logs:      %s@,Packages:  %s@,Total:     %s@]"
    (human_size r.base) (human_size r.builds) (human_size r.docs)
    (human_size r.jtw) (human_size r.solutions) (human_size r.logs)
    (human_size r.packages) (human_size r.total)
