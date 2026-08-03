let src =
  Logs.Src.create "day11.universe_manifest"
    ~doc:"Per-snapshot universe manifests read/write"

module Log = (val Logs.src_log src)

type t = { hash : string; packages : string list }

let dir snapshot_dir = Fpath.(snapshot_dir / "universes")
let index_path snapshot_dir = Fpath.(dir snapshot_dir / "index.json")

let manifest_path snapshot_dir hash =
  Fpath.(dir snapshot_dir / (hash ^ ".json"))

let write_all ~snapshot_dir universes =
  match Bos.OS.Dir.create ~path:true (dir snapshot_dir) with
  | Error _ as e -> e
  | Ok _ -> (
      let hashes = List.map fst universes in
      let index =
        `Assoc [ ("universes", `List (List.map (fun h -> `String h) hashes)) ]
      in
      match
        Bos.OS.File.write (index_path snapshot_dir)
          (Yojson.Safe.to_string index)
      with
      | Error _ as e -> e
      | Ok () ->
          List.fold_left
            (fun acc (hash, packages) ->
              match acc with
              | Error _ as e -> e
              | Ok () ->
                  let json =
                    `Assoc
                      [
                        ("universe_hash", `String hash);
                        ( "packages",
                          `List (List.map (fun p -> `String p) packages) );
                      ]
                  in
                  Bos.OS.File.write
                    (manifest_path snapshot_dir hash)
                    (Yojson.Safe.to_string json))
            (Ok ()) universes)

let read_index ~snapshot_dir =
  match Bos.OS.File.read (index_path snapshot_dir) with
  | Error _ -> []
  | Ok s -> (
      try
        let json = Yojson.Safe.from_string s in
        let open Yojson.Safe.Util in
        json |> member "universes" |> to_list |> List.map to_string
      with exn ->
        Log.warn (fun f ->
            f "universes/index.json parse failed (%s): %s"
              (Fpath.to_string (index_path snapshot_dir))
              (Printexc.to_string exn));
        [])

let read_manifest ~snapshot_dir ~hash =
  match Bos.OS.File.read (manifest_path snapshot_dir hash) with
  | Error _ -> None
  | Ok s -> (
      try
        let json = Yojson.Safe.from_string s in
        let open Yojson.Safe.Util in
        let hash = json |> member "universe_hash" |> to_string in
        let packages =
          json |> member "packages" |> to_list |> List.map to_string
        in
        Some { hash; packages }
      with exn ->
        Log.warn (fun f ->
            f "universes/%s.json parse failed (%s): %s" hash
              (Fpath.to_string (manifest_path snapshot_dir hash))
              (Printexc.to_string exn));
        None)
