let run ~sw env ~bundle ~container_id =
  let cmd =
    Bos.Cmd.(v "runc" % "run" % "-b" % Fpath.to_string bundle % container_id)
  in
  let log_file = Fpath.(bundle / "runc.log") in
  Day11_sys.Sudo.run ~output_file:log_file ~sw env cmd

let delete ~sw env container_id =
  let cmd = Bos.Cmd.(v "runc" % "delete" % "-f" % container_id) in
  Day11_sys.Sudo.run ~sw env cmd |> Result.map (fun _run -> ())
