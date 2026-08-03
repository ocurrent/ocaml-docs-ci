let save_manifest path ~universe_hash ~packages =
  let json =
    `Assoc
      [
        ("universe_hash", `String universe_hash);
        ("packages", `List (List.map (fun s -> `String s) packages));
      ]
  in
  Bos.OS.File.write path (Yojson.Safe.to_string json)

let load_manifest path =
  match Bos.OS.File.read path with
  | Error _ as e -> e
  | Ok data -> (
      try
        let json = Yojson.Safe.from_string data in
        let open Yojson.Safe.Util in
        let hash = json |> member "universe_hash" |> to_string in
        let packages =
          json |> member "packages" |> to_list |> List.map to_string
        in
        Ok (hash, packages)
      with exn ->
        Rresult.R.error_msgf "Universe.load_manifest: %s"
          (Printexc.to_string exn))

let write_package_refs ~pkg_html_dir ~universe_hashes =
  let json =
    `Assoc
      [ ("universes", `List (List.map (fun s -> `String s) universe_hashes)) ]
  in
  let path = Fpath.(pkg_html_dir / "universes.json") in
  Bos.OS.File.write path (Yojson.Safe.to_string json)

let collect_referenced ~html_dir =
  let p_dir = Fpath.(html_dir / "p") in
  let p_dir_s = Fpath.to_string p_dir in
  if not (Sys.file_exists p_dir_s) then []
  else
    let refs = Hashtbl.create 64 in
    (* Scan html/p/{pkg}/{ver}/universes.json *)
    let scan_pkg pkg_name =
      let pkg_dir = Filename.concat p_dir_s pkg_name in
      try
        Sys.readdir pkg_dir
        |> Array.iter (fun ver ->
               let univ_file =
                 Filename.concat (Filename.concat pkg_dir ver) "universes.json"
               in
               if Sys.file_exists univ_file then
                 try
                   let data =
                     In_channel.with_open_text univ_file In_channel.input_all
                   in
                   let json = Yojson.Safe.from_string data in
                   let open Yojson.Safe.Util in
                   json
                   |> member "universes"
                   |> to_list
                   |> List.iter (fun h -> Hashtbl.replace refs (to_string h) ())
                 with _ -> ())
      with Sys_error _ -> ()
    in
    (try Sys.readdir p_dir_s |> Array.iter scan_pkg with Sys_error _ -> ());
    Hashtbl.fold (fun k () acc -> k :: acc) refs []

let gc ~html_dir =
  let u_dir = Fpath.(html_dir / "u") in
  let u_dir_s = Fpath.to_string u_dir in
  if not (Sys.file_exists u_dir_s) then 0
  else
    let referenced = collect_referenced ~html_dir in
    let ref_set = Hashtbl.create 64 in
    List.iter (fun h -> Hashtbl.replace ref_set h ()) referenced;
    let deleted = ref 0 in
    (try
       Sys.readdir u_dir_s
       |> Array.iter (fun name ->
              if not (Hashtbl.mem ref_set name) then (
                Bos.OS.Dir.delete ~recurse:true Fpath.(u_dir / name) |> ignore;
                incr deleted))
     with Sys_error _ -> ());
    !deleted
