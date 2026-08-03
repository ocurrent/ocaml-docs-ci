type cached_solution = {
  package : OpamPackage.t;
  result : Day11_solution.Solve_result.t;
  cache_key : string option;
}

type cached_failure = {
  package : OpamPackage.t;
  error : string;
  examined : OpamPackage.Name.Set.t;
  cache_key : string option;
}

type cache_entry =
  | Cached_solution of cached_solution
  | Cached_failure of cached_failure

let cache_key_of = function
  | Cached_solution s -> s.cache_key
  | Cached_failure f -> f.cache_key

let is_cache_key_valid ~expected entry =
  match (expected, cache_key_of entry) with
  | None, _ -> true
  | Some _, None -> true (* legacy entry, accept *)
  | Some e, Some k -> e = k

let examined_to_json examined =
  `List
    (OpamPackage.Name.Set.fold
       (fun n acc -> `String (OpamPackage.Name.to_string n) :: acc)
       examined [])

let examined_of_json json =
  let open Yojson.Safe.Util in
  json
  |> to_list
  |> List.map to_string
  |> List.map OpamPackage.Name.of_string
  |> OpamPackage.Name.Set.of_list

(* Always write the [package] field for back-compat with consumers
   that still parse it from the body. The [cache_key] field is
   optional — only emitted when caller set it. *)
let save path entry =
  let with_key fields cache_key =
    match cache_key with
    | Some k -> ("cache_key", `String k) :: fields
    | None -> fields
  in
  let json =
    match entry with
    | Cached_solution { package; result; cache_key } ->
        `Assoc
          (with_key
             [
               ("package", `String (OpamPackage.to_string package));
               ("result", Day11_solution.Solve_result.to_json result);
             ]
             cache_key)
    | Cached_failure { package; error; examined; cache_key } ->
        `Assoc
          (with_key
             [
               ("failed", `Bool true);
               ("package", `String (OpamPackage.to_string package));
               ("error", `String error);
               ("examined", examined_to_json examined);
             ]
             cache_key)
  in
  let data = Yojson.Safe.to_string json in
  Bos.OS.File.write path data

(* Package name comes from the JSON body if present, otherwise from
   the filename — ocaml-docs-ci writes envelope-style files with no
   [package] field, relying on the filename ([<name>.<ver>.json]) to
   identify the entry. *)
let package_from_filename file_path =
  let basename = Fpath.basename file_path in
  let stem =
    if Filename.check_suffix basename ".json" then
      Filename.chop_suffix basename ".json"
    else basename
  in
  OpamPackage.of_string stem

let load file_path =
  match Bos.OS.File.read file_path with
  | Error _ as e -> e
  | Ok data -> (
      try
        let json = Yojson.Safe.from_string data in
        let open Yojson.Safe.Util in
        let package =
          match json |> member "package" |> to_string_option with
          | Some s -> OpamPackage.of_string s
          | None -> package_from_filename file_path
        in
        let cache_key = json |> member "cache_key" |> to_string_option in
        match json |> member "failed" |> to_bool_option with
        | Some true ->
            let error = json |> member "error" |> to_string in
            let examined = json |> member "examined" |> examined_of_json in
            Ok (Cached_failure { package; error; examined; cache_key })
        | _ -> (
            match
              Day11_solution.Solve_result.of_json (json |> member "result")
            with
            | Ok result -> Ok (Cached_solution { package; result; cache_key })
            | Error _ as e -> e)
      with exn -> Error (`Msg (Printexc.to_string exn)))

let reuse_solutions ?expected_cache_key ?rekey_to ?(yield = fun () -> ())
    ~solutions_cache_dir ~previous_dir ~changed_packages ~packages () =
  let reused = ref 0 in
  List.iter
    (fun pkg_name ->
      yield ();
      let cache_file = Fpath.(solutions_cache_dir / (pkg_name ^ ".json")) in
      (* In rekey mode the caller passes exactly the targets it knows to
       be missing or stale, so an existing (stale) file is overwritten;
       in hardlink mode preserve the historical skip-if-present. *)
      let skip =
        rekey_to = None && Sys.file_exists (Fpath.to_string cache_file)
      in
      if not skip then
        let prev_file = Fpath.(previous_dir / (pkg_name ^ ".json")) in
        if Sys.file_exists (Fpath.to_string prev_file) then
          match load prev_file with
          | Error _ -> ()
          | Ok entry
            when not (is_cache_key_valid ~expected:expected_cache_key entry) ->
              ()
          | Ok entry -> (
              let examined =
                match entry with
                | Cached_solution s -> s.result.examined
                | Cached_failure f -> f.examined
              in
              if
                OpamPackage.Name.Set.is_empty
                  (OpamPackage.Name.Set.inter examined changed_packages)
              then
                match rekey_to with
                | None -> (
                    try
                      Unix.link
                        (Fpath.to_string prev_file)
                        (Fpath.to_string cache_file);
                      incr reused
                    with Unix.Unix_error _ -> (
                      match Bos.OS.File.read prev_file with
                      | Ok data -> (
                          match Bos.OS.File.write cache_file data with
                          | Ok () -> incr reused
                          | Error _ -> ())
                      | Error _ -> ()))
                | Some new_key -> (
                    (* Re-stamp with the destination snapshot's cache key so
                 the consumer's [is_cache_key_valid] accepts the entry.
                 Failures are carried too: a failure whose examined set
                 is untouched by the changed packages is provably still
                 a failure, and re-attempting it is the {e expensive}
                 kind of solve (exhaustive search). Consumers decide
                 whether a carried failure short-circuits the re-solve
                 (ocaml-docs-ci's partition does, on a strict key
                 match) or is merely informational. *)
                    let entry' =
                      match entry with
                      | Cached_solution s ->
                          Cached_solution { s with cache_key = Some new_key }
                      | Cached_failure f ->
                          Cached_failure { f with cache_key = Some new_key }
                    in
                    match save cache_file entry' with
                    | Ok () -> incr reused
                    | Error _ -> ())))
    packages;
  !reused

(* ── Tool-solve caching ─────────────────────────────────────────
   Doc-tool solves (odoc-driver + one odoc per compiler) use the same
   cache-entry envelope as package solutions, stored per snapshot
   under [tool_solutions/]. The compiler pin rides in the {e filename}
   ([<pkg>@<compiler>.json] — [load] reads the package from the JSON
   body, so the stem is free-form); the cache key captures only the
   repo state, so one key covers every entry in the dir. *)

let tool_solutions_dirname = "tool_solutions"

let tool_cache_key ~repos =
  let sorted =
    List.sort compare (List.map (fun (path, sha) -> path ^ "@" ^ sha) repos)
  in
  Digest.to_hex (Digest.string (String.concat "\n" ("tool-v1" :: sorted)))

let find_previous_sha_dir base ~current_sha =
  match Bos.OS.Dir.contents base with
  | Error _ -> None
  | Ok entries -> (
      let dirs =
        List.filter_map
          (fun p ->
            let name = Fpath.basename p in
            if name = current_sha then None
            else
              match Bos.OS.Dir.exists p with
              | Ok true ->
                  let mtime =
                    try (Unix.stat (Fpath.to_string p)).Unix.st_mtime
                    with _ -> 0.0
                  in
                  Some (p, mtime)
              | _ -> None)
          entries
      in
      match List.sort (fun (_, a) (_, b) -> compare b a) dirs with
      | (p, _) :: _ -> Some p
      | [] -> None)
