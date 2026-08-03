let src_log = Logs.Src.create "day11.layer.import" ~doc:"Layer import"

module Log = (val Logs.src_log src_log)

let from_docker ~sw env ~image ~layer_dir =
  let fs_dir = Fpath.(layer_dir / "fs") in
  (try
     Eio.Path.mkdirs ~exists_ok:true ~perm:0o755
       Eio.Path.(env#fs / Fpath.to_string fs_dir)
   with _ -> ());
  let fs_s = Fpath.to_string fs_dir in
  (* Create a stopped container from the image *)
  Log.info (fun m -> m "Creating container from %s" image);
  let create_run =
    Day11_sys.Run.run ~sw env Bos.Cmd.(v "docker" % "create" % image) None
  in
  match create_run.status with
  | `Signaled n -> Rresult.R.error_msgf "docker create signaled %d" n
  | `Exited n when n <> 0 ->
      Rresult.R.error_msgf "docker create failed (exit %d): %s" n
        create_run.errors
  | _ -> (
      let container_id = String.trim create_run.output in
      Log.info (fun m -> m "Exporting container %s" container_id);
      (* Export and extract with sudo to preserve ownership. [-p]
     (--same-permissions) is essential: without it, tar applies the
     process umask to extracted directories, so dirs touched during the
     image build come out 0700 instead of 0755 — making /usr/bin (etc.)
     non-traversable by the non-root build user, which fails every build
     with "exec /usr/bin/env: permission denied". *)
      let export_run =
        Day11_sys.Run.run ~sw env
          Bos.Cmd.(
            v "sh"
            % "-c"
            % Printf.sprintf "docker export %s | sudo tar -xp -C %s"
                container_id fs_s)
          None
      in
      (* Always clean up the temporary container *)
      let _rm =
        Day11_sys.Run.run ~sw env
          Bos.Cmd.(v "docker" % "rm" % "-f" % container_id)
          None
      in
      match export_run.status with
      | `Exited 0 ->
          Log.info (fun m -> m "Imported %s to %a" image Fpath.pp layer_dir);
          Ok ()
      | `Exited n ->
          Rresult.R.error_msgf "docker export failed (exit %d): %s" n
            export_run.errors
      | `Signaled n -> Rresult.R.error_msgf "docker export signaled %d" n)
