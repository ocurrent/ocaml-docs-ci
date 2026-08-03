let src = Logs.Src.create "day11.build.build_layer" ~doc:"Package build"

module Log = (val Logs.src_log src)
module Build = Day11_opam_layer.Build
module Layer = Day11_layer.Layer

let mkdir path = Bos.OS.Dir.create ~path:true path |> ignore

(* Re-exports for backward compatibility — the helpers now live in
   Container_backend. *)
let opam_build_cleanup = Container_backend.opam_build_cleanup
let opam_build_spec = Container_backend.opam_build_spec
let opam_build_prep_upper = Container_backend.opam_build_prep_upper

(** Read the build result from an existing layer. *)
let result_of_layer env layer (node : Build.t) =
  if Layer.is_ok env layer then Types.Success node
  else Types.Failure (Build.dir_name node)

let exit_code_of (run : Day11_sys.Run.t) =
  match run.status with `Exited n -> n | `Signaled n -> 128 + n

(** Format the transitive dependency tree of a build node, deduped by build_hash
    and ordered by package name, for inclusion at the top of layer.log. The list
    shows the actual layer stack mounted in the container, which is what differs
    between universes — so diff'ing two layer.log headers immediately reveals
    why two universes' builds disagree (different fmt version, different
    compiler, etc.). *)
let format_deps_block (node : Build.t) =
  let seen = Hashtbl.create 32 in
  let acc = ref [] in
  let rec walk (b : Build.t) =
    if not (Hashtbl.mem seen b.hash) then (
      Hashtbl.replace seen b.hash ();
      List.iter walk b.deps;
      acc := b :: !acc)
  in
  List.iter walk node.Build.deps;
  let sorted =
    List.sort
      (fun (a : Build.t) (b : Build.t) -> OpamPackage.compare a.pkg b.pkg)
      (List.rev !acc)
  in
  let lines =
    List.map
      (fun (d : Build.t) ->
        Printf.sprintf "  %-50s %s"
          (OpamPackage.to_string d.pkg)
          (String.sub d.hash 0 (min 12 (String.length d.hash))))
      sorted
  in
  Printf.sprintf "=== DEPENDENCIES (%d transitive) ===\n%s\n"
    (List.length sorted) (String.concat "\n" lines)

(** Persist everything we know about the build BEFORE attempting it, so a failed
    build remains diagnosable.

    Writes [build.json] (with [installed_libs]/[installed_docs] empty — those
    need a successful build to scan), the [base] symlink, the [opam-repository/]
    slice, and the runc [config.json] template. None of these need the build to
    have succeeded; they describe its inputs.

    The presence-of-[layer.json] convention still marks success; this function
    never touches [layer.json]. *)
let record_input env ~layer ~node ~benv ?patches ?strategy
    ?(snapshot_repos = []) () =
  let _ = env in
  let layer_dir = Layer.dir layer in
  (* Symlink to the base layer dir so a human inspecting this layer
     can [ls -L base/] and find the rootfs the build started from.
     Best-effort: if the symlink can't be created (e.g. base is on
     a different filesystem), the recorded [base_image] field below
     still identifies which image was used. *)
  let base_link = Fpath.(layer_dir / "base") in
  (try
     Unix.symlink
       (Fpath.to_string benv.Types.base.dir)
       (Fpath.to_string base_link)
   with Unix.Unix_error _ -> ());
  let cmd = match strategy with Some s -> s.Types.cmd | None -> "" in
  let build_meta : Day11_opam_layer.Build_meta.t =
    {
      package = OpamPackage.to_string node.Build.pkg;
      deps =
        List.map
          (fun (d : Build.t) ->
            {
              Day11_opam_layer.Build_meta.pkg = OpamPackage.to_string d.pkg;
              hash = d.hash;
            })
          node.Build.deps;
      stack = Container_backend.collect_transitive_dep_hashes node;
      build_deps =
        List.sort String.compare
          (List.map OpamPackage.to_string
             (Container_backend.collect_transitive_dep_pkgs node));
      installed_libs = [];
      installed_docs = [];
      patches =
        (match patches with
        | Some p -> Patches.patch_filenames p node.Build.pkg
        | None -> []);
      base_image = benv.Types.base.image;
      cmd;
      universe = Day11_solution.Universe.to_string node.Build.universe;
    }
  in
  let _ = Day11_opam_layer.Build_meta.save layer_dir build_meta in
  let slice_written =
    if snapshot_repos <> [] then
      let patch_files =
        match patches with
        | Some p -> List.map Fpath.v (Patches.patches_for p node.Build.pkg)
        | None -> []
      in
      match
        Day11_opam_layer.Opam_repo.snapshot_to_layer ~layer_dir
          ~opam_repositories:snapshot_repos ~patches:patch_files node.Build.pkg
      with
      | Ok () -> true
      | Error _ -> false
    else false
  in
  (* Write a runc-runnable [config.json] template. Mounts the
     per-layer opam-repository slice (when one was written) at
     [/home/opam/.opam/repo/default]; the rootfs path is left as
     [<rootfs>] for the caller to substitute with an
     overlay-merged dir built from the base image plus this layer's
     transitive deps. *)
  match strategy with
  | None -> ()
  | Some s ->
      let slice_mount =
        if slice_written then
          [
            Day11_container.Mount.bind_ro
              ~src:(Fpath.to_string Fpath.(layer_dir / "opam-repository"))
              "/home/opam/.opam/repo/default";
          ]
        else []
      in
      let spec =
        Container_backend.opam_build_spec ~cmd:s.Types.cmd ~mounts:slice_mount
          ~uid:benv.Types.uid ~gid:benv.gid ()
      in
      let _ =
        Day11_container.Oci_spec.write_template
          Fpath.(layer_dir / "config.json")
          spec
      in
      ()

(** Persist the result of a build attempt.

    [layer.log] is always written so failed builds remain diagnosable.
    [layer.json] is written ONLY for successful builds ([exit_code = 0]); its
    presence is the success marker — [{!Day11_layer.Layer.is_ok}] returns true
    iff this file exists. Failure tracking also lives in
    [<os_dir>/layer_status.jsonl] (see {!Day11_layer.Layer_status}), recording
    exit_status without polluting the layer cache.

    On success, also re-writes [build.json] to fill in [installed_libs] and
    [installed_docs] — both fields require the build to have actually run and
    produced [fs/]. The other fields were already populated by {!record_input}
    pre-build. *)
let record_attempt env ~layer ~node ~benv ~timing ?patches
    (run : Day11_sys.Run.t) =
  let exit_code = exit_code_of run in
  let _ =
    Bos.OS.File.write (Layer.log_path layer)
      (Printf.sprintf "%s=== STDOUT ===\n%s\n=== STDERR ===\n%s\n"
         (format_deps_block node) run.output run.errors)
  in
  if exit_code = 0 then (
    let dep_hashes = List.map (fun (d : Build.t) -> d.hash) node.Build.deps in
    let disk_usage =
      match Day11_sys.Util.dir_size (Layer.dir layer) with
      | Ok size -> size
      | Error _ -> 0
    in
    let meta : Day11_layer.Meta.t =
      {
        exit_status = 0;
        parent_hashes = dep_hashes;
        uid = benv.Types.uid;
        gid = benv.gid;
        base_hash = benv.base.hash;
        disk_usage;
        timing;
        created_at = "";
        failed_dep = None;
      }
    in
    let _ = Day11_layer.Meta.save env (Layer.meta_path layer) meta in
    let layer_dir = Layer.dir layer in
    (* Re-load the input-side build.json written by [record_input],
       fill in the post-build scan results, save again. Falling back
       to a fresh record if the load fails keeps things robust if a
       caller invoked [record_attempt] without [record_input]. *)
    let bm =
      match Day11_opam_layer.Build_meta.load layer_dir with
      | Ok bm -> bm
      | Error _ ->
          {
            package = OpamPackage.to_string node.Build.pkg;
            deps =
              List.map
                (fun (d : Build.t) ->
                  {
                    Day11_opam_layer.Build_meta.pkg =
                      OpamPackage.to_string d.pkg;
                    hash = d.hash;
                  })
                node.Build.deps;
            stack = Container_backend.collect_transitive_dep_hashes node;
            build_deps =
              List.sort String.compare
                (List.map OpamPackage.to_string
                   (Container_backend.collect_transitive_dep_pkgs node));
            installed_libs = [];
            installed_docs = [];
            patches =
              (match patches with
              | Some p -> Patches.patch_filenames p node.Build.pkg
              | None -> []);
            base_image = benv.Types.base.image;
            cmd = "";
            universe = Day11_solution.Universe.to_string node.Build.universe;
          }
    in
    let bm =
      {
        bm with
        installed_libs = Day11_opam_layer.Installed_files.scan_libs ~layer_dir;
        installed_docs = Day11_opam_layer.Installed_files.scan_docs ~layer_dir;
      }
    in
    let _ = Day11_opam_layer.Build_meta.save layer_dir bm in
    (* Start the LRU clock at build time. Without this a layer carries
        no [last_used] sentinel until something re-uses it, and a sweep
        in between has nothing to date it by. *)
    Day11_layer.Last_used.touch env layer_dir;
    ());
  let os_dir = Fpath.parent (Layer.dir layer) in
  Day11_layer.Layer_status.append ~os_dir ~hash:(Layer.hash layer)
    ~exit_status:exit_code;
  exit_code

(** Container setup itself failed (couldn't even run the build). Just log the
    outcome to {!Day11_layer.Layer_status}; don't write any layer.json. *)
let record_setup_failure ~layer =
  let os_dir = Fpath.parent (Layer.dir layer) in
  Day11_layer.Layer_status.append ~os_dir ~hash:(Layer.hash layer)
    ~exit_status:1

(** Main entry point. *)
let build ~sw env (benv : Types.build_env) ~opam_repositories ?snapshot_repos
    ?mounts ?patches ?build_dirs ?prep_upper
    ?(on_extract = fun ~layer_dir:_ ~success:_ -> ()) (node : Build.t) ?strategy
    () =
  (* If the caller didn't explicitly supply [snapshot_repos], fall
     back to the in-flight [opam_repositories] (rerun's case) — both
     describe the source of opam metadata for this build. *)
  let snapshot_repos =
    match snapshot_repos with Some r -> r | None -> opam_repositories
  in
  let os_dir = benv.os_dir in
  let pkg_str = OpamPackage.to_string node.pkg in
  let layer_name = Build.dir_name node in
  let layer = Build.layer ~os_dir node in
  if Layer.exists env layer then (
    (* Layer dir with [layer.json] means a previous attempt
       succeeded — content-addressed, deterministic, durable. Failed
       attempts leave only a [layer.log] (no layer.json), so they
       don't trigger this short-circuit and can be re-attempted by
       OCurrent's rebuild flow. *)
    Log.info (fun m -> m "Cache hit: %s (%s)" pkg_str layer_name);
    Day11_layer.Last_used.touch env (Layer.dir layer);
    result_of_layer env layer node)
  else (
    Log.info (fun m -> m "Building %s (%s)" pkg_str layer_name);
    let layer_dir = Layer.dir layer in
    let lock_file = Fpath.(os_dir / (layer_name ^ ".lock")) in
    (* Clear any residue from a prior failed attempt at this hash.
       Without this, [Container_backend.build]'s [mv upper target_fs]
       lands {b inside} an existing [target_fs] (POSIX mv semantics
       for source→existing-directory) and silently fails on the
       second-retry case where [target_fs/upper/] is already
       populated. The actual install ends up in the temp upper, then
       gets rm -rf'd in cleanup. Net effect: layer.json says success
       but the captured [fs/] is empty. See menhir.20260209
       cascade-from-failure repro. *)
    if Sys.file_exists (Fpath.to_string layer_dir) then
      ignore (Day11_sys.Sudo.rm_rf ~sw env layer_dir);
    (* Resolve the default strategy here so [record_attempt] can
       record the actual cmd that ran. Both backends already fall
       back to [opam_build_strategy ?patches node.pkg] when no
       strategy is passed; lifting the resolution doesn't change
       behaviour. *)
    let resolved_strategy =
      match strategy with
      | Some s -> s
      | None -> Container_backend.opam_build_strategy ?patches node.pkg
    in
    let _lock_result =
      Day11_sys.Dir_lock.with_lock ~marker_file:(Fpath.v "layer.json")
        ~lock_file layer_dir (fun ~set_temp_log_path:_ _dir ->
          mkdir layer_dir;
          (* Persist the build's input description {b before} we run
             it. If the build fails (or the host crashes mid-build),
             the layer dir still has [build.json], the [base]
             symlink, the [opam-repository/] slice, and a
             [config.json] template — enough to drive [day11 debug]
             or a manual [runc run]. *)
          record_input env ~layer ~node ~benv ?patches
            ~strategy:resolved_strategy ~snapshot_repos ();
          let target_fs = Layer.fs layer in
          match
            Container_backend.build ~sw env benv ~opam_repositories ?mounts
              ?patches ?build_dirs ?prep_upper ~strategy:resolved_strategy node
              ~target_fs ()
          with
          | Ok (run, timing) ->
              let exit_code =
                record_attempt env ~layer ~node ~benv ~timing ?patches run
              in
              Log.info (fun m ->
                  m "Build %s: exit %d (%.1fs)" pkg_str exit_code run.time);
              on_extract ~layer_dir ~success:(exit_code = 0);
              Ok ()
          | Error (`Msg e) ->
              Log.err (fun m -> m "Build %s failed: %s" pkg_str e);
              Printf.eprintf "BUILD ERROR %s: %s\n%!" pkg_str e;
              record_setup_failure ~layer;
              on_extract ~layer_dir:(Layer.dir layer) ~success:false;
              Ok ())
    in
    if Layer.is_ok env layer then result_of_layer env layer node
    else Types.Failure layer_name)
