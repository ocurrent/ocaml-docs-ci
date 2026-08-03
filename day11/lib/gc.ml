type result = { total : int; kept : int; deleted : int }

let log fmt = Printf.ksprintf (fun msg -> Printf.printf "[gc] %s\n%!" msg) fmt

let rm_rf path =
  let ret =
    Sys.command (Printf.sprintf "rm -rf %s 2>/dev/null" (Filename.quote path))
  in
  if ret <> 0 then
    ignore (Sys.command (Printf.sprintf "sudo rm -rf %s" (Filename.quote path)))

let list_dirs dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    try
      Sys.readdir dir
      |> Array.to_list
      |> List.filter (fun name -> Sys.is_directory (Filename.concat dir name))
    with _ -> []
  else []

let gc_build_layers ~os_dir ~referenced =
  let referenced_set = Hashtbl.create (List.length referenced) in
  List.iter (fun r -> Hashtbl.replace referenced_set r ()) referenced;
  let all =
    list_dirs os_dir
    |> List.filter (fun name ->
        String.length name > 6 && String.sub name 0 6 = "build-")
  in
  let total = List.length all in
  let deleted = ref 0 in
  List.iter
    (fun name ->
      if not (Hashtbl.mem referenced_set name) then (
        log "Deleting unreferenced layer: %s" name;
        rm_rf (Filename.concat os_dir name);
        (* Also remove lock file *)
        let lock = Filename.concat os_dir (name ^ ".lock") in
        if Sys.file_exists lock then rm_rf lock;
        incr deleted))
    all;
  { total; kept = total - !deleted; deleted = !deleted }

let gc_odoc_store ~os_dir ~referenced_universes =
  let referenced_set = Hashtbl.create (List.length referenced_universes) in
  List.iter (fun u -> Hashtbl.replace referenced_set u ()) referenced_universes;
  let store = Filename.concat os_dir "odoc-store" in
  let deleted = ref 0 in
  let total = ref 0 in
  (* GC odoc-out/u/ and html/u/ *)
  List.iter
    (fun subdir ->
      let u_dir = Filename.concat (Filename.concat store subdir) "u" in
      let universes = list_dirs u_dir in
      List.iter
        (fun uhash ->
          incr total;
          if not (Hashtbl.mem referenced_set uhash) then (
            log "Deleting unreferenced universe %s from %s/u/" uhash subdir;
            rm_rf (Filename.concat u_dir uhash);
            incr deleted))
        universes)
    [ "odoc-out"; "html" ];
  { total = !total; kept = !total - !deleted; deleted = !deleted }

let gc_stale_temp_dirs () =
  let tmp = Filename.get_temp_dir_name () in
  let stale =
    list_dirs tmp
    |> List.filter (fun name ->
        String.length name > 10 && String.sub name 0 10 = "day11_run_")
  in
  List.iter
    (fun name ->
      let merged = Filename.concat (Filename.concat tmp name) "merged" in
      ignore (Sys.command (Printf.sprintf "sudo umount %s 2>/dev/null" merged));
      rm_rf (Filename.concat tmp name))
    stale;
  List.length stale
