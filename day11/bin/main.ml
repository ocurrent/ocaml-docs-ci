(** day11 CLI — build and document OCaml packages *)

open Cmdliner

let default_term =
  let doc = "Build and document OCaml packages" in
  let info = Cmd.info "day11" ~doc ~man:[
    `S Manpage.s_description;
    `P "Build, document, and manage OCaml package health checks.";
    `P "Use 'day11 batch' to build all packages.";
    `P "Use 'day11 status' to check build results.";
    `P "Use 'day11 debug TARGET' to debug a failed build.";
  ] in
  let term = Term.(const 0) in
  Cmd.group ~default:term info [
    Cmd_profile.cmd;
    Cmd_build.cmd;
    Cmd_batch.cmd;
    Cmd_diff.cmd;
    Cmd_snapshots.cmd;
    Cmd_results.cmd;
    Cmd_status.cmd;
    Cmd_query.cmd;
    Cmd_failures.cmd;
    Cmd_cascade.cmd;
    Cmd_disk.cmd;
    Cmd_rerun.cmd;
    Cmd_rdeps.cmd;
    Cmd_gc.cmd;
    Cmd_strip_opaque.cmd;
    Cmd_report.cmd;
    Cmd_log.cmd;
    Cmd_debug.cmd;
  ]

let () =
  (* Ensure temp dirs go to /tmp, not cwd or git's tmp *)
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  (* Honour DAY11_LOG=debug, info, warning, or error env var. *)
  (match Sys.getenv_opt "DAY11_LOG" with
   | Some "debug" -> Logs.set_level (Some Logs.Debug)
   | Some "info" -> Logs.set_level (Some Logs.Info)
   | Some "warning" -> Logs.set_level (Some Logs.Warning)
   | Some "error" -> Logs.set_level (Some Logs.Error)
   | _ -> Logs.set_level (Some Logs.Warning));
  Logs.set_reporter (Logs_fmt.reporter ());
  exit (Cmd.eval' default_term)
