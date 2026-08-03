type t = { repos : (string * string) list; key : string; created : string }

let git_head_sha repo_path =
  let cmd =
    Printf.sprintf "git -C %s rev-parse HEAD 2>/dev/null"
      (Filename.quote repo_path)
  in
  let ic = Unix.open_process_in cmd in
  let sha = try String.trim (input_line ic) with _ -> "unknown" in
  ignore (Unix.close_process_in ic);
  sha

let compute_key repos =
  let shas = List.map snd repos in
  let combined = String.concat ":" shas in
  Digest.string combined |> Digest.to_hex |> fun s -> String.sub s 0 12

let now_iso8601 () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let current (profile : Profile.t) =
  let repos =
    List.map (fun path -> (path, git_head_sha path)) profile.opam_repositories
  in
  let key = compute_key repos in
  { repos; key; created = now_iso8601 () }

let to_json t =
  `Assoc
    [
      ( "repos",
        `List
          (List.map
             (fun (path, sha) ->
               `Assoc [ ("path", `String path); ("commit", `String sha) ])
             t.repos) );
      ("key", `String t.key);
      ("created", `String t.created);
    ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let repos =
      json
      |> member "repos"
      |> to_list
      |> List.map (fun r ->
             (r |> member "path" |> to_string, r |> member "commit" |> to_string))
    in
    let key = json |> member "key" |> to_string in
    let created = json |> member "created" |> to_string in
    Ok { repos; key; created }
  with exn ->
    Rresult.R.error_msgf "Snapshot.of_json: %s" (Printexc.to_string exn)

let save dir t =
  let path = Fpath.(dir / "repos.json") in
  try
    ignore (Bos.OS.Dir.create ~path:true dir);
    Bos.OS.File.write path (Yojson.Safe.pretty_to_string (to_json t))
  with exn ->
    Rresult.R.error_msgf "Snapshot.save: %s" (Printexc.to_string exn)

let load dir =
  let path = Fpath.(dir / "repos.json") in
  match Bos.OS.File.read path with
  | Error _ as e -> e
  | Ok data -> (
      try of_json (Yojson.Safe.from_string data)
      with exn ->
        Rresult.R.error_msgf "Snapshot.load: %s" (Printexc.to_string exn))

let solutions_dir dir = Fpath.(dir / "solutions")
let packages_dir dir = Fpath.(dir / "packages")
let runs_dir dir = Fpath.(dir / "runs")
let status_json dir = Fpath.(dir / "status.json")
