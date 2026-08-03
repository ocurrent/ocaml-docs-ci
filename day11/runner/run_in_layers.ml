let src =
  Logs.Src.create "day11.runner.run_in_layers"
    ~doc:"Run a command in a layered container"

module Log = (val Logs.src_log src)

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e
let mkdir path = Bos.OS.Dir.create ~path:true path |> ignore

(** Like [timed] but also stores elapsed time in a ref. *)
let timed_to name dst f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  dst := elapsed;
  if elapsed > 0.1 then Log.info (fun m -> m "%s: %.3fs" name elapsed);
  r

let run ~sw env ~(base : Day11_layer.Base.t) ~build_dirs ?prep_upper
    (spec : Day11_container.Oci_spec.t) =
  let t_total = Unix.gettimeofday () in
  let t_merge = ref 0. in
  let t_prep = ref 0. in
  let t_mount = ref 0. in
  let t_runc = ref 0. in
  let t_umount = ref 0. in
  let t_cleanup = ref 0. in
  let base_fs = Fpath.add_seg base.dir "fs" in
  let temp_dir =
    let tmp = Fpath.v (Filename.get_temp_dir_name ()) in
    let name = Printf.sprintf "day11_run_%06x" (Random.bits () land 0xffffff) in
    let p = Fpath.(tmp / name) in
    Bos.OS.Dir.create ~path:true p |> ignore;
    p
  in
  let upper = Fpath.(temp_dir / "upper") in
  let work = Fpath.(temp_dir / "work") in
  let merged = Fpath.(temp_dir / "merged") in
  let lower = Fpath.(temp_dir / "lower") in
  List.iter mkdir [ upper; work; merged ];
  (* Hybrid lowerdir layout. The classic mount(2) options string is
     capped at PAGE_SIZE (typically 4096 bytes), which limits how
     many dep layers we can pass as separate lowerdirs. The hybrid:

     1. Try to fit all dep layers as individual lowerdirs in the
        mount options. For typical opam packages (<60 deps with
        long /home/jjl25/cache/... paths, ~160 with /c-style short
        paths) this works directly.

     2. If the option string would exceed the budget, keep as many
        layers as possible as separate lowerdirs and cp-merge the
        excess into one [lower/] dir, which becomes one extra
        lowerdir.

     Multi-lower is correct: per the kernel docs, overlayfs DOES
     merge directories across multiple lowers — see
     docs.kernel.org/filesystems/overlayfs.html: "Where both upper
     and lower objects are directories, a merged directory is
     formed".

     BUT [trusted.overlay.opaque=y] on a lower layer's directory
     DOES shadow layers below it for that directory (and similarly
     for whiteout char-devices). Layers captured from a previous
     build can carry [opaque] on any directory that the build
     modified destructively — see
     {!Day11_opam_build.Container_backend.build} where the upper is
     stripped of [trusted.overlay.opaque] xattrs before promotion
     to [fs/], to keep the layer cache neutral when it's later
     used as a lower. *)
  (* Mark every dep layer as recently used for LRU eviction. *)
  List.iter (Day11_layer.Last_used.touch env) build_dirs;
  let dep_entry_cost d =
    String.length (Fpath.to_string Fpath.(d / "fs")) + 1 (* colon *)
  in
  let fixed_overhead =
    String.length "lowerdir="
    + String.length (Fpath.to_string base_fs)
    + String.length ",upperdir="
    + String.length (Fpath.to_string upper)
    + String.length ",workdir="
    + String.length (Fpath.to_string work)
  in
  let merged_overhead =
    String.length (Fpath.to_string lower) + 1
    (* colon *)
  in
  let available = 4000 - fixed_overhead in
  let separate_dirs, to_merge_dirs =
    Day11_layer.Stack.plan_lowerdir ~available ~merged_overhead
      ~entry_cost:dep_entry_cost build_dirs
  in
  let did_merge = to_merge_dirs <> [] in
  if did_merge then (
    mkdir lower;
    let merge_result =
      timed_to
        (Printf.sprintf "stack.merge (%d of %d build layers)"
           (List.length to_merge_dirs)
           (List.length build_dirs))
        t_merge
        (fun () ->
          Day11_layer.Stack.merge ~sw env ~layer_dirs:to_merge_dirs
            ~target:lower)
    in
    match merge_result with
    | Ok () -> ()
    | Error (`Msg e) -> Log.err (fun m -> m "stack.merge failed: %s" e));
  (* layer_fs_dirs is the list of dep lowers in the order used in
     the overlayfs mount (separate first, then merged-lower if any).
     It is also passed to [prep_upper] so domain-aware callers can
     read per-dep state from the lowers if they need to. *)
  let layer_fs_dirs =
    List.map (fun d -> Fpath.(d / "fs")) separate_dirs
    @ if did_merge then [ lower ] else []
  in
  let cleanup_internals () =
    if did_merge then ignore (Day11_sys.Sudo.rm_rf ~sw env lower);
    ignore (Day11_sys.Sudo.rm_rf ~sw env work);
    ignore (Day11_sys.Sudo.rm_rf ~sw env merged);
    ignore (Bos.OS.File.delete Fpath.(temp_dir / "config.json"))
  in
  (* Caller-supplied prep work on the upper, before the mount.
     This is where domain-aware callers seed switch state, chown
     home directories, mkdir mount points, etc. *)
  (match prep_upper with
  | None -> ()
  | Some f ->
      timed_to "prep_upper" t_prep (fun () ->
          f ~upper ~lowers:(layer_fs_dirs @ [ base_fs ])));
  (* Mount overlay with all layers as separate lowers *)
  let overlay_lowers = layer_fs_dirs @ [ base_fs ] in
  let* () =
    timed_to "overlay mount" t_mount (fun () ->
        Day11_container.Overlay.mount ~sw env ~lower:overlay_lowers ~upper ~work
          ~target:merged)
  in
  (* Run container — always clean up overlay + container *)
  let run_result =
    Fun.protect
      ~finally:(fun () ->
        timed_to "overlay umount" t_umount (fun () ->
            ignore (Day11_container.Overlay.umount ~sw env merged)))
      (fun () ->
        let* () =
          Day11_container.Oci_spec.write ~root:(Fpath.to_string merged) temp_dir
            spec
        in
        let container_id =
          Printf.sprintf "day11-%s-%d"
            (String.sub (Fpath.basename temp_dir) 0
               (min 20 (String.length (Fpath.basename temp_dir))))
            (Unix.getpid ())
        in
        ignore (Day11_container.Runc.delete ~sw env container_id);
        Fun.protect
          ~finally:(fun () ->
            ignore (Day11_container.Runc.delete ~sw env container_id))
          (fun () ->
            timed_to "runc run" t_runc (fun () ->
                Day11_container.Runc.run ~sw env ~bundle:temp_dir ~container_id)))
  in
  (* Always clean up internals — only upper survives *)
  timed_to "cleanup internals" t_cleanup (fun () -> cleanup_internals ());
  let timing =
    [
      ("merge", !t_merge);
      ("prep_upper", !t_prep);
      ("overlay_mount", !t_mount);
      ("runc_run", !t_runc);
      ("overlay_umount", !t_umount);
      ("cleanup", !t_cleanup);
      ("total", Unix.gettimeofday () -. t_total);
    ]
  in
  match run_result with
  | Ok run -> Ok (run, upper, timing)
  | Error _ as e ->
      ignore (Day11_sys.Sudo.rm_rf ~sw env temp_dir);
      e
