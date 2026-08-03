type stage = Build | Doc | Tool

type lock_info = {
  stage : stage;
  package : string;
  version : string;
  universe : string option;
  pid : int;
  start_time : float;
  layer_name : string option;
  temp_log_path : string option;
}

let locks_subdir = "locks"

let parse_lock_filename filename =
  if not (Filename.check_suffix filename ".lock") then None
  else
    let base = Filename.chop_suffix filename ".lock" in
    let parse_pkg_ver_universe rest =
      match String.rindex_opt rest '-' with
      | Some i when String.length rest - i - 1 = 32 -> (
          let pkg_ver = String.sub rest 0 i in
          let universe = String.sub rest (i + 1) (String.length rest - i - 1) in
          match String.rindex_opt pkg_ver '.' with
          | None -> None
          | Some j ->
              let package = String.sub pkg_ver 0 j in
              let version =
                String.sub pkg_ver (j + 1) (String.length pkg_ver - j - 1)
              in
              Some (package, version, Some universe))
      | _ -> (
          match String.rindex_opt rest '.' with
          | None -> None
          | Some i ->
              let package = String.sub rest 0 i in
              let version =
                String.sub rest (i + 1) (String.length rest - i - 1)
              in
              Some (package, version, None))
    in
    if String.length base > 6 && String.sub base 0 6 = "build-" then
      let rest = String.sub base 6 (String.length base - 6) in
      parse_pkg_ver_universe rest
      |> Option.map (fun (package, version, universe) ->
             (Build, package, version, universe))
    else if String.length base > 4 && String.sub base 0 4 = "doc-" then
      let rest = String.sub base 4 (String.length base - 4) in
      parse_pkg_ver_universe rest
      |> Option.map (fun (package, version, universe) ->
             (Doc, package, version, universe))
    else if String.length base > 5 && String.sub base 0 5 = "tool-" then
      let rest = String.sub base 5 (String.length base - 5) in
      match String.rindex_opt rest '-' with
      | Some i ->
          let name = String.sub rest 0 i in
          let ocaml_ver =
            String.sub rest (i + 1) (String.length rest - i - 1)
          in
          if String.contains ocaml_ver '.' then
            Some (Tool, name, "0", Some ocaml_ver)
          else Some (Tool, rest, "0", None)
      | None -> Some (Tool, rest, "0", None)
    else None

let is_lock_held lock_path =
  try
    let fd = Unix.openfile lock_path [ Unix.O_RDONLY ] 0o644 in
    let held =
      try
        Unix.lockf fd Unix.F_TEST 0;
        false
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) | Unix.Unix_error (Unix.EACCES, _, _)
      ->
        true
    in
    Unix.close fd;
    held
  with Unix.Unix_error _ -> false

let list_active ~cache_dir =
  let locks_dir = Filename.concat cache_dir locks_subdir in
  if not (Sys.file_exists locks_dir) then []
  else
    try
      Sys.readdir locks_dir
      |> Array.to_list
      |> List.filter (fun name -> Filename.check_suffix name ".lock")
      |> List.filter_map (fun filename ->
             let path = Filename.concat locks_dir filename in
             match parse_lock_filename filename with
             | None -> None
             | Some (stage, package, version, universe) ->
                 if is_lock_held path then
                   try
                     let content =
                       In_channel.with_open_text path In_channel.input_all
                     in
                     let lines = String.split_on_char '\n' content in
                     match lines with
                     | pid_str :: time_str :: rest ->
                         let pid = int_of_string (String.trim pid_str) in
                         let start_time =
                           float_of_string (String.trim time_str)
                         in
                         let layer_name =
                           match rest with
                           | s :: _ when String.trim s <> "" ->
                               Some (String.trim s)
                           | _ -> None
                         in
                         let temp_log_path =
                           match rest with
                           | _ :: s :: _ when String.trim s <> "" ->
                               Some (String.trim s)
                           | _ -> None
                         in
                         Some
                           {
                             stage;
                             package;
                             version;
                             universe;
                             pid;
                             start_time;
                             layer_name;
                             temp_log_path;
                           }
                     | _ -> None
                   with _ -> None
                 else None)
    with _ -> []

let cleanup_stale ~cache_dir =
  let locks_dir = Filename.concat cache_dir locks_subdir in
  if Sys.file_exists locks_dir then
    try
      Sys.readdir locks_dir
      |> Array.iter (fun filename ->
             if Filename.check_suffix filename ".lock" then
               let path = Filename.concat locks_dir filename in
               if not (is_lock_held path) then
                 try Unix.unlink path with _ -> ())
    with _ -> ()
