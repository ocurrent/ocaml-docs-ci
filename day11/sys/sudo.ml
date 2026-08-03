let src = Logs.Src.create "day11.sys.sudo" ~doc:"Privileged execution"

module Log = (val Logs.src_log src)

let run ?output_file ~sw env cmd =
  let sudo_cmd = Bos.Cmd.(v "sudo" %% cmd) in
  Log.debug (fun m -> m "Running: %a" Bos.Cmd.pp sudo_cmd);
  try Ok (Run.run ~sw env sudo_cmd output_file)
  with exn ->
    Log.warn (fun m ->
        m "sudo failed: %a: %s" Bos.Cmd.pp sudo_cmd (Printexc.to_string exn));
    Rresult.R.error_msgf "sudo: %s" (Printexc.to_string exn)

let rm_rf ~sw env path =
  let cmd = Bos.Cmd.(v "rm" % "-rf" % Fpath.to_string path) in
  run ~sw env cmd |> Result.map (fun _run -> ())
