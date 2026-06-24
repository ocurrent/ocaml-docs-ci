type t = {
  hash : string;
  dir : Fpath.t;
}

(* Manual doc-format version. Bump when day11's doc-generation logic,
   HTML layout, or output convention changes *without* a doc-tool change
   — it's the only input that captures pure-code changes. *)
let version = "v2"  (* v2: per-universe doc nodes (compile/link per (bh,U)) *)

(* The epoch hash identifies a set of docs all produced by the same doc
   toolchain. It folds in [version] plus the resolved *versions* of the
   doc-format-determining tool packages: odoc, odoc-driver (voodoo lives
   inside it), odoc-md, sherlodoc and odig. We key on package versions
   ([name.version]) rather than the tools' content-addressed build
   hashes deliberately: a build hash sits at the top of the tool's full
   dependency closure, so it moved whenever *any* transitive dep bumped
   in opam-repository — minting a new epoch (and forcing a full re-link
   of the world) every day or two for changes that don't affect odoc's
   output at all. Versions move only when a tool that actually shapes the
   HTML changes. Master/overlay builds are still captured, because those
   encode the git SHA in the version string (e.g. [odoc.3.2.0+master.<sha>]).
   Per-package inputs (the documented packages' versions, individual
   builds) are deliberately NOT included — those update incrementally
   within an epoch; folding them in would mint a new epoch on every
   package bump. [inputs] is sorted+deduped so ordering doesn't matter. *)
let compute ~inputs =
  let key = String.concat ":" (List.sort_uniq String.compare inputs) in
  Printf.sprintf "%s:%s" version key
  |> Digest.string |> Digest.to_hex

let create ~base_dir hash =
  let dir = Fpath.(base_dir / ("epoch-" ^ hash)) in
  Bos.OS.Dir.create ~path:true dir |> ignore;
  { hash; dir }

let promote ~base_dir t =
  let link = Fpath.(base_dir / "html-live") in
  let link_s = Fpath.to_string link in
  (* Relative target, so the symlink resolves regardless of where the
     base dir is mounted: the daemon sees it under HOME, but the web
     server (Caddy) serves the same tree mounted at a different path
     (e.g. /srv). [t.dir] is [base_dir/epoch-<hash>] and the link lives
     in [base_dir], so the relative target is "epoch-<hash>/html". *)
  let target =
    Filename.concat (Filename.basename (Fpath.to_string t.dir)) "html" in
  (try Unix.unlink link_s with Unix.Unix_error _ -> ());
  Unix.symlink target link_s

let current ~base_dir =
  let link = Fpath.(base_dir / "html-live") in
  let link_s = Fpath.to_string link in
  try
    let target = Unix.readlink link_s in
    (* [target] is "epoch-<hash>/html" (relative) — or, for a legacy
       absolute link, a path ending the same way. The epoch dir is the
       basename of the target's parent. Rebuild the full dir under
       [base_dir] so callers (e.g. gc) get an absolute path. *)
    let name = Filename.basename (Filename.dirname target) in
    if String.length name > 6 && String.sub name 0 6 = "epoch-" then
      let hash = String.sub name 6 (String.length name - 6) in
      Some { hash; dir = Fpath.(base_dir / name) }
    else None
  with Unix.Unix_error _ -> None

(* [to_gc ~base_dir ~keep] is the *selection* half of gc: the epoch dirs
   that should be reclaimed, keeping the [keep] most-recent plus the
   currently-live one. Pure and fast (readdir + stat) — the caller is
   responsible for the actual (potentially very slow, multi-million-file)
   deletion, which must not run on a latency-sensitive event loop. *)
let to_gc ~base_dir ~keep =
  let base_s = Fpath.to_string base_dir in
  let entries =
    try Sys.readdir base_s |> Array.to_list
    with Sys_error _ -> []
  in
  let epochs = List.filter_map (fun name ->
    if String.length name > 6 && String.sub name 0 6 = "epoch-" then
      let dir = Fpath.(base_dir / name) in
      let mtime =
        try (Unix.stat (Fpath.to_string dir)).Unix.st_mtime
        with _ -> 0.0
      in
      Some (name, dir, mtime)
    else None
  ) entries in
  let sorted = List.sort (fun (_, _, a) (_, _, b) -> compare b a) epochs in
  let to_delete = if List.length sorted > keep then
    List.filteri (fun i _ -> i >= keep) sorted
  else [] in
  (* Never delete the currently-live epoch, even if it's older than the
     [keep] most-recent — e.g. after a deliberate rollback-promote to a
     known-good older epoch. *)
  let to_delete = match current ~base_dir with
    | Some live ->
      List.filter (fun (_, dir, _) -> not (Fpath.equal dir live.dir)) to_delete
    | None -> to_delete
  in
  List.map (fun (_, dir, _) -> dir) to_delete

let gc ~base_dir ~keep =
  let dirs = to_gc ~base_dir ~keep in
  List.iter (fun dir ->
    Bos.OS.Dir.delete ~recurse:true dir |> ignore
  ) dirs;
  List.length dirs
