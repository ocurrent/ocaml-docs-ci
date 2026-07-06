module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool
module Layer = Day11_layer.Layer
type build = Build.t

let load_exit_status env layer_json =
  match Day11_layer.Meta.load env layer_json with
  | Ok { exit_status; _ } -> Some exit_status
  | Error _ -> None

let build_env_of_meta ~os_dir ~cache_dir:_ ~image
    (meta : Day11_layer.Meta.t) =
  let base_dir = Day11_opam_build.Base.base_dir_of_os_dir os_dir in
  let base : Day11_layer.Base.t = {
    hash = meta.base_hash;
    dir = base_dir;
    image;
  } in
  Day11_opam_build.Types.make_build_env ~base ~os_dir
    ~uid:meta.uid ~gid:meta.gid ()

let rerun ~sw env ~os_dir ~cache_dir node =
  let layer = Build.layer ~os_dir node in
  let layer_dir = Layer.dir layer in
  match Day11_layer.Meta.load env (Layer.meta_path layer) with
  | Error (`Msg e) ->
    Day11_opam_build.Types.Failure e
  | Ok { exit_status = 0; _ } ->
    Day11_opam_build.Types.Success node
  | Ok meta ->
    (* Read [base_image] from the layer's [build.json] sidecar so the
       rerun reconstructs the same base. Fall back to "" — the cached
       base layer at [os_dir/base] is still mounted by hash, so a
       warm cache rerun works without the image string. *)
    let image =
      match Day11_opam_layer.Build_meta.load layer_dir with
      | Ok bm -> bm.base_image
      | Error _ -> ""
    in
    let benv = build_env_of_meta ~os_dir ~cache_dir ~image meta in
    let opam_repo = Fpath.(layer_dir / "opam-repository") in
    let opam_repos =
      if Bos.OS.Dir.exists opam_repo |> Result.get_ok
      then [ opam_repo ]
      else []
    in
    ignore (Day11_sys.Sudo.rm_rf ~sw env layer_dir);
    Day11_opam_build.Build_layer.build ~sw env benv
      ~opam_repositories:opam_repos node ()

let cascade ~sw env ~os_dir ~cache_dir nodes =
  let rerun_count = ref 0 in
  List.iter (fun (node : build) ->
    let layer = Build.layer ~os_dir node in
    match load_exit_status env (Layer.meta_path layer) with
    | Some (-1) ->
      let all_deps_ok = List.for_all (fun (dep : build) ->
        let dep_layer = Build.layer ~os_dir dep in
        load_exit_status env (Layer.meta_path dep_layer) = Some 0
      ) node.deps in
      if all_deps_ok then begin
        ignore (rerun ~sw env ~os_dir ~cache_dir node);
        incr rerun_count
      end
    | _ -> ()
  ) nodes;
  !rerun_count
