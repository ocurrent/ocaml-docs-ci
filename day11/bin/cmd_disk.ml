(** disk command: show disk usage breakdown *)

open Cmdliner

let run profile_name profile_dir =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) ->
      let os_dir = paths.os_dir in
      let cache_dir = paths.cache_dir in
      let report = Day11_lib.Disk_usage.scan ~os_dir ~cache_dir in
      Fmt.pr "%a\n" Day11_lib.Disk_usage.pp report;
      0

let cmd =
  let info = Cmd.info "disk" ~doc:"Show disk usage breakdown" in
  let term = Term.(const run $ Common.profile_term $ Common.profile_dir_term) in
  Cmd.v info term
