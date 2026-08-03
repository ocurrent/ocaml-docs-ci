let generate ~packages_dir =
  let dir_s = Fpath.to_string packages_dir in
  if not (Sys.file_exists dir_s) then []
  else
    Sys.readdir dir_s
    |> Array.to_list
    |> List.filter (fun name ->
           let path = Filename.concat dir_s name in
           try Sys.is_directory path with Sys_error _ -> false)
    |> List.filter (fun pkg_str ->
           let entries = History.read ~packages_dir ~pkg_str in
           match entries with
           | [] -> false
           | latest :: _ -> latest.status = "success")

let save path packages =
  let json = `List (List.map (fun s -> `String s) packages) in
  Bos.OS.File.write path (Yojson.Safe.to_string json)

let load path =
  match Bos.OS.File.read path with
  | Error _ as e -> e
  | Ok data -> (
      try
        let json = Yojson.Safe.from_string data in
        Ok (Yojson.Safe.Util.to_list json |> List.map Yojson.Safe.Util.to_string)
      with exn ->
        Rresult.R.error_msgf "Package_list.load: %s" (Printexc.to_string exn))
