let src =
  Logs.Src.create "day11.build.opam_init_base"
    ~doc:"opam-init seed layer for native builds"

module Log = (val Logs.src_log src)
module Layer = Day11_layer.Layer

let mkdir path = Bos.OS.Dir.create ~path:true path |> ignore

(* -- Hash computation ---------------------------------------------------- *)

let opam_version () =
  match
    Bos.OS.Cmd.run_out Bos.Cmd.(v "opam" % "--version") |> Bos.OS.Cmd.out_string
  with
  | Ok (s, _) -> String.trim s
  | Error _ -> "unknown"

let compute_hash ~opam_repositories =
  let repo_ids = List.map Fpath.to_string opam_repositories in
  let sorted = List.sort String.compare repo_ids in
  let inputs =
    "opam-init-v1"
    :: opam_version ()
    :: "--bare --no-setup --disable-sandboxing"
    :: "--switch default --empty"
    :: sorted
  in
  Day11_layer.Hash.of_strings inputs

let layer_dir_name hash = "opam-init-" ^ Day11_layer.Dir.name hash

(* -- Build the layer ----------------------------------------------------- *)

let run_host_cmd ~sw env cmd =
  let s = String.concat " " (Bos.Cmd.to_list cmd) in
  Log.info (fun m -> m "+ %s" s);
  let run = Day11_sys.Run.run ~sw env cmd None in
  match run.status with
  | `Exited 0 -> Ok ()
  | `Exited n ->
      Rresult.R.error_msgf "%s exited %d\n%s\n%s" s n run.output run.errors
  | `Signaled n -> Rresult.R.error_msgf "%s signaled %d" s n

let ( let* ) r f = match r with Ok v -> f v | Error _ as e -> e

(* Write the generic layer.json so Layer.exists returns true after build. *)
let write_layer_meta env ~layer ~hash =
  let uid = Unix.getuid () and gid = Unix.getgid () in
  let disk_usage =
    match Day11_sys.Util.dir_size (Layer.dir layer) with
    | Ok n -> n
    | Error _ -> 0
  in
  let meta : Day11_layer.Meta.t =
    {
      exit_status = 0;
      parent_hashes = [];
      uid;
      gid;
      base_hash = "opam-init";
      disk_usage;
      timing = Day11_layer.Meta.empty_timing;
      created_at = "";
      failed_dep = None;
    }
  in
  let _ = Day11_layer.Meta.save env (Layer.meta_path layer) meta in
  let _ =
    Bos.OS.File.write (Layer.log_path layer)
      (Printf.sprintf "opam-init base layer hash=%s\n" hash)
  in
  ()

let build ~sw env ~os_dir ~opam_repositories =
  let hash = compute_hash ~opam_repositories in
  let layer_dir = Fpath.(os_dir / layer_dir_name hash) in
  let layer : Layer.t = { hash; dir = layer_dir } in
  if Layer.exists env layer then Ok layer
  else (
    Log.info (fun m -> m "Building opam-init base layer %s" hash);
    let temp_dir = Bos.OS.Dir.tmp "day11_opam_init_%s" |> Result.get_ok in
    let home = Fpath.(temp_dir / "home" / "opam") in
    let opamroot = Fpath.(home / ".opam") in
    mkdir home;
    let root_flag = [ "--root"; Fpath.to_string opamroot ] in
    let add_args base args = List.fold_left Bos.Cmd.( % ) base args in
    (* 1. opam init --bare: creates the global root. NAME and ADDRESS
       are positional in opam init; we always name the repo 'default'
       so it matches container-build convention. When no repos are
       supplied, omit the positional args and let opam fetch its
       default. *)
    let init_base =
      add_args
        Bos.Cmd.(
          v "opam"
          % "init"
          % "--bare"
          % "--no-setup"
          % "--disable-sandboxing"
          % "-y")
        root_flag
    in
    let init_args =
      match opam_repositories with
      | [] -> init_base
      | first :: _ ->
          (* "-k local default <path>" *)
          Bos.Cmd.(
            init_base % "-k" % "local" % "default" % Fpath.to_string first)
    in
    let* () = run_host_cmd ~sw env init_args in
    (* 2. Create an empty 'default' switch inside this root. *)
    let switch_args =
      add_args
        Bos.Cmd.(v "opam" % "switch" % "create" % "default" % "--empty" % "-y")
        root_flag
    in
    let* () = run_host_cmd ~sw env switch_args in
    (* 3. Move the populated temp home into the layer's fs/. *)
    mkdir layer_dir;
    let fs_dir = Layer.fs layer in
    let _ =
      Sys.command
        (Printf.sprintf "mv %s %s"
           (Filename.quote (Fpath.to_string temp_dir))
           (Filename.quote (Fpath.to_string fs_dir)))
    in
    write_layer_meta env ~layer ~hash;
    ignore (Bos.OS.Path.delete ~recurse:true temp_dir);
    Log.info (fun m -> m "opam-init base layer written: %a" Fpath.pp layer_dir);
    Ok layer)

let ensure ~sw env ~os_dir ?(opam_repositories = []) () =
  build ~sw env ~os_dir ~opam_repositories
