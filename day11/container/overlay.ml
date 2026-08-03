(* Dedupe paths preserving first-seen order. Overlayfs rejects
   duplicate lowerdir entries — the mount fails with EINVAL and
   dmesg logs "conflicting lowerdir path"; from userspace this
   surfaces as "Too many levels of symbolic links" and the merged
   dir ends up empty (later runc init fails with confusing errors
   like "exec: /usr/bin/env: stat /usr/bin/env: no such file or
   directory" because the rootfs isn't there).

   Callers in the doc pipeline compose [lower] from multiple
   sources (build_deps_layers, own compile_layer, transitive
   doc-deps' compile layers) and overlap is hard to avoid at the
   call site, so we dedupe here. *)
let dedup_preserving_order paths =
  let seen = Hashtbl.create (List.length paths) in
  List.fold_left
    (fun acc p ->
      let key = Fpath.to_string p in
      if Hashtbl.mem seen key then acc
      else (
        Hashtbl.add seen key ();
        p :: acc))
    [] paths
  |> List.rev

let mount ~sw env ~lower ~upper ~work ~target =
  let lower = dedup_preserving_order lower in
  let lower_str = List.map Fpath.to_string lower |> String.concat ":" in
  let opts =
    Printf.sprintf "lowerdir=%s,upperdir=%s,workdir=%s" lower_str
      (Fpath.to_string upper) (Fpath.to_string work)
  in
  let cmd =
    Bos.Cmd.(
      v "mount"
      % "-t"
      % "overlay"
      % "overlay"
      % "-o"
      % opts
      % Fpath.to_string target)
  in
  Day11_sys.Sudo.run ~sw env cmd |> Result.map (fun _run -> ())

let umount ~sw env target =
  let cmd = Bos.Cmd.(v "umount" % Fpath.to_string target) in
  Day11_sys.Sudo.run ~sw env cmd |> Result.map (fun _run -> ())
