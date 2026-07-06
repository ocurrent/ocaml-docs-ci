module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
type build = Build.t

type session = {
  temp_dir : Fpath.t;
  os_dir : Fpath.t;
  build : build;
  pkg : OpamPackage.t;
  uid : int;
  gid : int;
}

let debug_env = [
  ("PATH", "/home/opam/.opam/default/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
  ("HOME", "/home/opam");
  ("OPAM_SWITCH_PREFIX", "/home/opam/.opam/default");
]

let setup ~sw env ~os_dir ?(keep = false) node =
  let cache_dir = Fpath.parent os_dir in
  let layer_dir = Build.dir ~os_dir node in
  let layer_json = Fpath.(layer_dir / "layer.json") in
  match Day11_layer.Meta.load env layer_json with
  | Error _ as e -> e
  | Ok meta ->
    (* Reconstruct base *)
    let base_dir = Base.base_dir_of_os_dir os_dir in
    let base : Day11_layer.Base.t = {
      hash = meta.base_hash; dir = base_dir; image = "";
    } in
    let uid = meta.uid and gid = meta.gid in
    (* Create temp dir *)
    let temp_dir =
      if keep then
        Fpath.(cache_dir / Printf.sprintf "debug-%s"
          (String.sub node.hash 0 (min 12 (String.length node.hash))))
      else
        Bos.OS.Dir.tmp "day11_debug_%s" |> Result.get_ok
    in
    let resuming = Bos.OS.Dir.exists Fpath.(temp_dir / "rootfs" / "home")
      |> Result.get_ok in
    Bos.OS.Dir.create ~path:true temp_dir |> ignore;
    if not resuming then begin
      let upper = Fpath.(temp_dir / "fs") in
      let work = Fpath.(temp_dir / "work") in
      let rootfs = Fpath.(temp_dir / "rootfs") in
      let merged_lower = Fpath.(temp_dir / "lower") in
      List.iter (fun d -> Bos.OS.Dir.create ~path:true d |> ignore)
        [ upper; work; rootfs ];
      (* Mirror {!Day11_runner.Run_in_layers.run}'s mount strategy so
         the debug container sees the exact same lowerdir sequence a
         production build would: [separate_dirs] each as their own
         lowerdir, a [merged_lower] for anything that would overflow
         the PAGE_SIZE limit, and [base_fs] at the bottom. Dep layers
         are collected via {!Container_backend.collect_transitive_dep_dirs}
         (DFS post-order, reversed — direct deps topmost), matching
         what [Build_layer.build] would pass. *)
      let build_dirs = Container_backend.collect_transitive_dep_dirs
        ~os_dir node in
      let base_dir = base.Day11_layer.Base.dir in
      let base_fs = Fpath.(base_dir / "fs") in
      let dep_entry_cost d =
        String.length (Fpath.to_string Fpath.(d / "fs")) + 1 in
      let fixed_overhead =
        String.length "lowerdir="
        + String.length (Fpath.to_string base_fs)
        + String.length ",upperdir=" + String.length (Fpath.to_string upper)
        + String.length ",workdir=" + String.length (Fpath.to_string work)
      in
      let merged_overhead =
        String.length (Fpath.to_string merged_lower) + 1 in
      let available = 4000 - fixed_overhead in
      let separate_dirs, to_merge_dirs =
        Day11_layer.Stack.plan_lowerdir
          ~available ~merged_overhead ~entry_cost:dep_entry_cost build_dirs
      in
      let did_merge = to_merge_dirs <> [] in
      if did_merge then begin
        Bos.OS.Dir.create ~path:true merged_lower |> ignore;
        match Day11_layer.Stack.merge ~sw env
                ~layer_dirs:to_merge_dirs ~target:merged_lower with
        | Ok () -> ()
        | Error (`Msg e) ->
          ignore (Day11_sys.Sudo.rm_rf ~sw env temp_dir);
          failwith e
      end;
      let layer_fs_dirs =
        List.map (fun d -> Fpath.(d / "fs")) separate_dirs
        @ (if did_merge then [ merged_lower ] else [])
      in
      let overlay_lowers = layer_fs_dirs @ [ base_fs ] in
      (* Dump switch-state into upper so opam sees deps as installed.
         Reading from the FIRST lower that has a packages dir matches
         what [Container_backend.opam_build_prep_upper] does. *)
      let switch = Types.switch in
      let switch_rel = Fpath.(v "home" / "opam" / ".opam" / switch
                              / ".opam-switch") in
      let packages_dirs = List.filter_map (fun dir ->
        let p = Fpath.(dir // Fpath.(v "packages")) in
        (* [dir] is already a [.../fs] path, so [packages_rel] is relative
           to [home/opam/...]. Pull the full rel from [switch_rel/packages]. *)
        ignore p;
        let packages_rel = Fpath.(switch_rel / "packages") in
        let abs = Fpath.(dir // packages_rel) in
        if Bos.OS.Dir.exists abs |> Result.value ~default:false then Some abs
        else None
      ) overlay_lowers in
      if packages_dirs <> [] then begin
        let state_dir = Fpath.(upper // switch_rel) in
        Bos.OS.Dir.create ~path:true state_dir |> ignore;
        Day11_opam_layer.Opamh.dump_state packages_dirs
          Fpath.(state_dir / "switch-state") |> ignore
      end;
      (* Mount overlay with the same lower sequence as production. *)
      (match Day11_container.Overlay.mount ~sw env
               ~lower:overlay_lowers ~upper ~work ~target:rootfs with
       | Ok () -> ()
       | Error (`Msg e) ->
         ignore (Day11_sys.Sudo.rm_rf ~sw env temp_dir);
         failwith e);
      (* Extract source *)
      let source_cmd = Printf.sprintf
        "opam source %s --dir=/home/opam/src"
        (OpamPackage.to_string node.pkg) in
      let spec = Day11_container.Oci_spec.make
        ~cwd:"/home/opam"
        ~hostname:"debug" ~env:debug_env ~network:true
        ~argv:[ "/usr/bin/env"; "bash"; "-c"; source_cmd ]
        ~uid ~gid () in
      ignore (Day11_container.Oci_spec.write
        ~root:(Fpath.to_string rootfs) temp_dir spec);
      let container_id = Printf.sprintf "debug-src-%d" (Unix.getpid ()) in
      ignore (Day11_container.Runc.delete ~sw env container_id);
      ignore (Day11_container.Runc.run ~sw env ~bundle:temp_dir ~container_id);
      ignore (Day11_container.Runc.delete ~sw env container_id)
    end;
    Ok { temp_dir; os_dir; build = node; pkg = node.pkg; uid; gid }

let run_in_session ~sw env session ~terminal ~argv =
  let rootfs = Fpath.(session.temp_dir / "rootfs") in
  let uid = session.uid and gid = session.gid in
  let spec = Day11_container.Oci_spec.make
    ~terminal
    ~cwd:"/home/opam/src"
    ~hostname:"debug" ~env:debug_env ~network:true
    ~argv ~uid ~gid () in
  ignore (Day11_container.Oci_spec.write
    ~root:(Fpath.to_string rootfs) session.temp_dir spec);
  let container_id = Printf.sprintf "debug-%d" (Unix.getpid ()) in
  ignore (Day11_container.Runc.delete ~sw env container_id);
  let result = match
    Day11_container.Runc.run ~sw env ~bundle:session.temp_dir ~container_id
  with
    | Ok run -> (match run.status with `Exited n -> n | `Signaled n -> 128 + n)
    | Error _ -> 1
  in
  ignore (Day11_container.Runc.delete ~sw env container_id);
  result

let run_interactive ~sw env session =
  let pkg_str = OpamPackage.to_string session.pkg in
  let cmd = Printf.sprintf
    "echo '==> Source: /home/opam/src'; \
     echo '==> Building %s'; \
     opam-build -v %s; \
     if [ $? -ne 0 ]; then \
       echo; echo '==> Build failed. Dropping to interactive shell.'; \
       echo '==> Run: opam-build -v %s   to retry'; echo; \
       exec bash -i; \
     fi"
    pkg_str pkg_str pkg_str
  in
  run_in_session ~sw env session ~terminal:true
    ~argv:[ "/usr/bin/env"; "bash"; "-c"; cmd ]

let run_command ~sw env session cmd =
  run_in_session ~sw env session ~terminal:false
    ~argv:[ "/usr/bin/env"; "bash"; "-c"; cmd ]

let teardown ~sw env session =
  let rootfs = Fpath.(session.temp_dir / "rootfs") in
  ignore (Day11_container.Overlay.umount ~sw env rootfs);
  ignore (Day11_sys.Sudo.rm_rf ~sw env session.temp_dir)
