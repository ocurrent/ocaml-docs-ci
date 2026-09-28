(* See .mli for the model. *)

let layer_html layer_dir = Fpath.(layer_dir / "html")
let marker_name = ".day11-layer"

(* Root-level file recording which support dir a layer (or an epoch) uses. *)
let support_marker = ".day11-support"
let ( let* ) = Result.bind
let msgf fmt = Fmt.kstr (fun s -> Error (`Msg s)) fmt
let exists p = Sys.file_exists (Fpath.to_string p)

let is_dir p =
  let s = Fpath.to_string p in
  Sys.file_exists s && Sys.is_directory s

let readdir p =
  try Sys.readdir (Fpath.to_string p) |> Array.to_list |> List.sort compare
  with Sys_error _ -> []

let read_trimmed p =
  match Bos.OS.File.read p with Ok s -> Some (String.trim s) | Error _ -> None

let rm_rf p =
  if exists p then
    Bos.OS.Path.delete ~must_exist:false ~recurse:true p
    |> Result.map_error (fun (`Msg m) -> `Msg m)
  else Ok ()

let mkdir_p p = Bos.OS.Dir.create ~path:true p |> Result.map ignore

let copy_file ~src ~dst =
  let* data = Bos.OS.File.read src in
  let* () = Bos.OS.File.write dst data in
  let perm = (Unix.stat (Fpath.to_string src)).Unix.st_perm in
  Unix.chmod (Fpath.to_string dst) perm;
  Ok ()

(* Hardlink [src] (a file, symlink or directory tree) to [dst], which must
   not exist. Falls back to copying when the two sit on different
   filesystems. *)
let rec link_tree ~src ~dst =
  let s = Fpath.to_string src and d = Fpath.to_string dst in
  match (Unix.lstat s).Unix.st_kind with
  | Unix.S_DIR ->
      let perm = (Unix.stat s).Unix.st_perm in
      Unix.mkdir d perm;
      List.fold_left
        (fun acc name ->
          let* () = acc in
          link_tree ~src:Fpath.(src / name) ~dst:Fpath.(dst / name))
        (Ok ()) (readdir src)
  | Unix.S_LNK ->
      Unix.symlink (Unix.readlink s) d;
      Ok ()
  | Unix.S_REG -> (
      try
        Unix.link s d;
        Ok ()
      with Unix.Unix_error ((Unix.EXDEV | Unix.EPERM | Unix.EMLINK), _, _) ->
        copy_file ~src ~dst)
  | _ -> Ok () (* sockets, fifos, devices: not part of an HTML tree *)

let link_tree ~src ~dst =
  try link_tree ~src ~dst
  with Unix.Unix_error (e, fn, arg) ->
    msgf "linking %a -> %a: %s(%s): %s" Fpath.pp src Fpath.pp dst fn arg
      (Unix.error_message e)

(* Move [src] to [dst] (which must not exist): a rename, or link+delete
   when they're on different filesystems. *)
let move ~src ~dst =
  try
    Unix.rename (Fpath.to_string src) (Fpath.to_string dst);
    Ok ()
  with Unix.Unix_error (Unix.EXDEV, _, _) ->
    let* () = link_tree ~src ~dst in
    rm_rf src

(* A name beside [p] that nothing else will pick, for staging a swap. *)
let sibling p ~tag =
  let dir, base = Fpath.split_base p in
  Fpath.add_seg dir
    (Printf.sprintf ".%s.%s-%d-%06x" (Fpath.to_string base) tag (Unix.getpid ())
       (Random.bits () land 0xffffff))

let capture ~src ~layer_dir ~support_root ~support_key =
  let dest = layer_html layer_dir in
  let* () = rm_rf dest in
  let* () = mkdir_p dest in
  let unit_roots, support =
    List.partition (fun n -> n = "p" || n = "u") (readdir src)
  in
  let* () =
    List.fold_left
      (fun acc name ->
        let* () = acc in
        move ~src:Fpath.(src / name) ~dst:Fpath.(dest / name))
      (Ok ()) unit_roots
  in
  let* () =
    let target = Fpath.(support_root / support_key) in
    if support = [] || exists target then Ok ()
    else
      (* Stage then rename, so a concurrent capture for the same toolchain
         either wins the rename or finds the dir already there. *)
      let* () = mkdir_p support_root in
      let staging = sibling target ~tag:"tmp" in
      let* () = mkdir_p staging in
      let* () =
        List.fold_left
          (fun acc name ->
            let* () = acc in
            move ~src:Fpath.(src / name) ~dst:Fpath.(staging / name))
          (Ok ()) support
      in
      match move ~src:staging ~dst:target with
      | Ok () -> Ok ()
      | Error _ when exists target -> rm_rf staging
      | Error _ as e -> e
  in
  let* () = Bos.OS.File.write Fpath.(dest / support_marker) support_key in
  rm_rf src

(* Units of a layer's HTML tree, as paths relative to its root:
   [p/<name>/<version>] and [u/<universe>/<name>/<version>]. *)
let units html =
  let dirs_under rel =
    List.filter_map
      (fun n ->
        let r = Fpath.(rel / n) in
        if is_dir Fpath.(html // r) then Some r else None)
      (readdir Fpath.(html // rel))
  in
  let p = Fpath.v "p" and u = Fpath.v "u" in
  let blessed = List.concat_map dirs_under (dirs_under p) in
  let unblessed =
    List.concat_map dirs_under (List.concat_map dirs_under (dirs_under u))
  in
  blessed @ unblessed

(* Link [src] (a unit in a layer) into place at [dst] in an epoch, with a
   marker naming [hash]. Built beside [dst] and renamed in, so readers see
   the old unit or the new one, never a half-linked tree. *)
let swap_in ~src ~dst ~hash =
  let* () = mkdir_p (Fpath.parent dst) in
  let staging = sibling dst ~tag:"tmp" in
  let* () = link_tree ~src ~dst:staging in
  let* () = Bos.OS.File.write Fpath.(staging / marker_name) hash in
  if exists dst then (
    let old = sibling dst ~tag:"old" in
    let* () = move ~src:dst ~dst:old in
    match move ~src:staging ~dst with
    | Ok () -> rm_rf old
    | Error _ as e ->
        (* Put the previous unit back rather than leave a hole. *)
        ignore (move ~src:old ~dst);
        ignore (rm_rf staging);
        e)
  else move ~src:staging ~dst

let publish_support ~epoch_html ~support_root html =
  if exists Fpath.(epoch_html / support_marker) then Ok ()
  else
    match read_trimmed Fpath.(html / support_marker) with
    | None -> Ok ()
    | Some key ->
        let src = Fpath.(support_root / key) in
        if not (is_dir src) then
          (* Nothing to link (yet); leave the epoch unmarked so a later
             layer tries again. *)
          Ok ()
        else
          let* () =
            List.fold_left
              (fun acc name ->
                let* () = acc in
                let dst = Fpath.(epoch_html / name) in
                if exists dst then Ok ()
                else link_tree ~src:Fpath.(src / name) ~dst)
              (Ok ()) (readdir src)
          in
          Bos.OS.File.write Fpath.(epoch_html / support_marker) key

type outcome = No_html | Published of { units : int; skipped : int }

let publish ~epoch_html ~support_root ~hash layer_dir =
  let html = layer_html layer_dir in
  if not (is_dir html) then Ok No_html
  else
    let* () = mkdir_p epoch_html in
    let* () = publish_support ~epoch_html ~support_root html in
    List.fold_left
      (fun acc rel ->
        let* n, s = acc in
        let dst = Fpath.(epoch_html // rel) in
        if read_trimmed Fpath.(dst / marker_name) = Some hash then Ok (n, s + 1)
        else
          let* () = swap_in ~src:Fpath.(html // rel) ~dst ~hash in
          Ok (n + 1, s))
      (Ok (0, 0))
      (units html)
    |> Result.map (fun (units, skipped) -> Published { units; skipped })

type stats = {
  layers : int;
  no_html : int;
  units : int;
  skipped : int;
  errors : (string * string) list;
}

let reconcile ~epoch_html ~support_root layers =
  let init = { layers = 0; no_html = 0; units = 0; skipped = 0; errors = [] } in
  List.fold_left
    (fun st (hash, layer_dir) ->
      let st = { st with layers = st.layers + 1 } in
      match publish ~epoch_html ~support_root ~hash layer_dir with
      | Ok No_html -> { st with no_html = st.no_html + 1 }
      | Ok (Published { units; skipped }) ->
          { st with units = st.units + units; skipped = st.skipped + skipped }
      | Error (`Msg m) -> { st with errors = (hash, m) :: st.errors })
    init layers
  |> fun st -> { st with errors = List.rev st.errors }
