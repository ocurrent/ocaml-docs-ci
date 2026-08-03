open Cmdliner

type t = {
  jobs : int;
  track_packages : string list;
  take_n_last_versions : int option;
}

let jobs =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 8)
  @@ Arg.info ~doc:"Number of parallel jobs" ~docv:"JOBS" [ "jobs"; "j" ]

let track_packages =
  Arg.value
  @@ Arg.opt Arg.(list string) []
  @@ Arg.info ~doc:"Filter the name of packages to track." ~docv:"PKGS"
       [ "filter" ]

let take_n_last_versions =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Limit the number of versions" ~docv:"LIMIT" [ "limit" ]

let v jobs track_packages take_n_last_versions =
  { jobs; track_packages; take_n_last_versions }

let cmdliner = Term.(const v $ jobs $ track_packages $ take_n_last_versions)
let jobs t = t.jobs
let track_packages t = t.track_packages
let take_n_last_versions t = t.take_n_last_versions
