open Cmdliner

type t = {
  jobs : int;
  track_packages : string list;
  take_n_last_versions : int option;
  doc_modules_max_depth : int;
  doc_modules_max_paths : int;
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

let positive_int =
  let parse s =
    match int_of_string_opt s with
    | Some n when n >= 1 -> Ok n
    | _ -> Error (`Msg (Printf.sprintf "%S: expected an integer >= 1" s))
  in
  Arg.conv (parse, Format.pp_print_int)

let doc_modules_max_depth =
  Arg.value
  @@ Arg.opt positive_int Day11_doc.Doc_modules.default_max_depth
  @@ Arg.info
       ~doc:
         "Module paths deeper than $(docv) are left out of the \
          doc-modules.jsonl.gz export (ocaml.org's module search)."
       ~docv:"DEPTH"
       [ "doc-modules-max-depth" ]

let doc_modules_max_paths =
  Arg.value
  @@ Arg.opt positive_int Day11_doc.Doc_modules.default_max_paths
  @@ Arg.info
       ~doc:
         "At most $(docv) module paths per package, shallowest first, go into \
          the doc-modules.jsonl.gz export (ocaml.org's module search)."
       ~docv:"COUNT"
       [ "doc-modules-max-paths" ]

let v jobs track_packages take_n_last_versions doc_modules_max_depth
    doc_modules_max_paths =
  {
    jobs;
    track_packages;
    take_n_last_versions;
    doc_modules_max_depth;
    doc_modules_max_paths;
  }

let cmdliner =
  Term.(
    const v
    $ jobs
    $ track_packages
    $ take_n_last_versions
    $ doc_modules_max_depth
    $ doc_modules_max_paths)

let jobs t = t.jobs
let track_packages t = t.track_packages
let take_n_last_versions t = t.take_n_last_versions
let doc_modules_max_depth t = t.doc_modules_max_depth
let doc_modules_max_paths t = t.doc_modules_max_paths
