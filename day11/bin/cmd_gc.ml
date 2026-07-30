(** gc command: reclaim disk space from the shared cache *)

open Cmdliner

let du cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim line
  with _ -> "?"

let human_bytes n =
  if n >= 1_000_000_000 then Printf.sprintf "%.1fG" (float n /. 1e9)
  else if n >= 1_000_000 then Printf.sprintf "%.1fM" (float n /. 1e6)
  else if n >= 1_000 then Printf.sprintf "%.1fK" (float n /. 1e3)
  else Printf.sprintf "%dB" n

let isatty = try Unix.isatty Unix.stderr with _ -> false

(* Scanning is the slow part (a stat + small JSON read per layer, ×591k),
   so show live progress on stderr. On a TTY redraw a single line in place;
   otherwise emit an occasional plain line so piped logs stay readable. *)
let show_progress ~os ~i ~n ~old ~deleted ~delete =
  let msg =
    if delete then
      Printf.sprintf "[gc] %s: %d/%d scanned, %d old, %d deleted" os i n old deleted
    else
      Printf.sprintf "[gc] %s: %d/%d scanned, %d old" os i n old
  in
  if isatty then Printf.eprintf "\r\027[K%s%!" msg
  else if n > 0 && (i mod 50_000 = 0 || i = n) then Printf.eprintf "%s\n%!" msg

let clear_progress () = if isatty then Printf.eprintf "\r\027[K%!"

let run profile_dir before_days delete show_du =
  Common.with_eio @@ fun ~sw:_ env ->
  let pdir = Common.resolve_profile_dir profile_dir in
  let cache_dir = Fpath.(pdir / "cache") in
  Printf.printf "=== Garbage Collection ===\n\n";
  Printf.printf "Cache: %s\n" (Fpath.to_string cache_dir);
  (* [du -sh] on the whole cache recursively stats every file under it —
     591k+ layer dirs *plus* the html epoch trees (millions of files
     each), and it ran twice per invocation. Off by default; the
     reclaimable figure below comes from layer metadata instead. *)
  if show_du then
    Printf.printf "  Size: %s\n"
      (du (Printf.sprintf "du -sh %s 2>/dev/null | cut -f1"
        (Fpath.to_string cache_dir)));
  Printf.printf "\n";
  (* 1. Clean stale temp dirs *)
  let n_stale = Day11_lib.Gc.gc_stale_temp_dirs () in
  if n_stale > 0 then
    Printf.printf "Cleaned %d stale overlay temp dirs\n\n" n_stale;
  (* 2. Scan layers by last-used time *)
  let cutoff = Unix.gettimeofday () -. (float before_days *. 86400.) in
  let os_dirs =
    match Bos.OS.Dir.contents cache_dir with
    | Error _ -> []
    | Ok entries ->
      List.filter (fun p ->
        let name = Fpath.basename p in
        name <> "base" && name <> "opam-build-bin" &&
        (Bos.OS.Dir.exists p |> Result.get_ok)
      ) entries
  in
  let total_layers = ref 0 in
  let old_layers = ref 0 in
  let old_size = ref 0 in
  let deleted_count = ref 0 in
  let deleted_size = ref 0 in
  List.iter (fun os_dir ->
    let os_name = Fpath.basename os_dir in
    let entries =
      try Sys.readdir (Fpath.to_string os_dir) |> Array.to_list
      with _ -> [] in
    let layers = List.filter (fun name ->
      (* Layer dirs: 12-char hex or build-<hex> (legacy) *)
      let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
      (String.length name = 12 && String.for_all is_hex name)
      || (String.length name > 6 && String.sub name 0 6 = "build-")
    ) entries in
    let n = List.length layers in
    total_layers := !total_layers + n;
    List.iteri (fun idx name ->
      let layer_dir = Fpath.(os_dir / name) in
      (* [effective], not [get]: a layer with no [last_used] sentinel
         falls back to its own [layer.json] mtime rather than epoch 0,
         so a freshly built layer that nothing has touched yet isn't
         read as infinitely old and deleted on the spot. Only residue
         with neither sentinel nor metadata scores 0.0. *)
      let last_used = match Day11_layer.Last_used.effective env layer_dir with
        | Some t -> t | None -> 0.0 in
      if last_used < cutoff then begin
        incr old_layers;
        (* Reclaimable bytes come from [disk_usage] in the layer's
           [layer.json] — recorded once at build time — not from a
           recursive [du] or from stat'ing the dir inode (which only
           reports the ~4 KB directory entry, not the layer contents).
           One small JSON read, and only for layers we'd actually free. *)
        let size =
          match Day11_layer.Meta.load env Fpath.(layer_dir / "layer.json") with
          | Ok (m : Day11_layer.Meta.t) -> m.disk_usage
          | Error _ -> 0 in
        old_size := !old_size + size;
        if delete then begin
          ignore (Sys.command (Printf.sprintf "sudo rm -rf %s"
            (Fpath.to_string layer_dir)));
          incr deleted_count;
          deleted_size := !deleted_size + size
        end
      end;
      (* Throttle the redraw to once every 1024 layers (and the last). *)
      let i = idx + 1 in
      if i mod 1024 = 0 || i = n then
        show_progress ~os:os_name ~i ~n ~old:!old_layers
          ~deleted:!deleted_count ~delete
    ) layers;
    clear_progress ();
    Printf.printf "  %s: %d layers\n" os_name n
  ) os_dirs;
  Printf.printf "\nTotal layers: %d\n" !total_layers;
  Printf.printf "Layers last used before %d days ago: %d (%s reclaimable)\n"
    before_days !old_layers (human_bytes !old_size);
  if delete then
    Printf.printf "Deleted: %d layers (%s freed)\n"
      !deleted_count (human_bytes !deleted_size)
  else if !old_layers > 0 then
    Printf.printf "Run with --delete to remove them.\n";
  if show_du then
    Printf.printf "\nCache after: %s\n"
      (du (Printf.sprintf "du -sh %s 2>/dev/null | cut -f1"
        (Fpath.to_string cache_dir)));
  0

let before_term =
  let doc = "Delete layers not used in the last N days (default 30)" in
  Arg.(value & opt int 30 & info [ "before" ] ~docv:"DAYS" ~doc)

let delete_term =
  let doc = "Actually delete old layers (default: report only)" in
  Arg.(value & flag & info [ "delete" ] ~doc)

let du_term =
  let doc = "Also report total cache size via [du -sh] before and after \
             (slow: recursively walks the whole cache, including the html \
             epoch trees). Off by default." in
  Arg.(value & flag & info [ "du" ] ~doc)

let cmd =
  let info = Cmd.info "gc"
    ~doc:"Reclaim disk space by removing old layers from the shared cache" in
  let term = Term.(const run $ Common.profile_dir_term
    $ before_term $ delete_term $ du_term) in
  Cmd.v info term
