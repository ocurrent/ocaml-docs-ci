(** Package patches: load, hash, and apply via opam-build --patch. *)

type t = {
  dir : Fpath.t;
  cache : (string, string list) Hashtbl.t;
  hash_cache : (string, string) Hashtbl.t;
}

let create dir =
  { dir; cache = Hashtbl.create 32; hash_cache = Hashtbl.create 32 }

let patches_for t pkg =
  let key = OpamPackage.to_string pkg in
  match Hashtbl.find_opt t.cache key with
  | Some ps -> ps
  | None ->
      let pkg_dir = Fpath.(t.dir / key) in
      let patches =
        if Bos.OS.Dir.exists pkg_dir |> Result.get_ok then
          match Bos.OS.Dir.contents pkg_dir with
          | Ok entries ->
              entries
              |> List.filter (fun p ->
                     Fpath.has_ext ".patch" p || Fpath.has_ext ".diff" p)
              |> List.map Fpath.to_string
              |> List.sort String.compare
          | Error _ -> []
        else []
      in
      Hashtbl.replace t.cache key patches;
      patches

let hash_for t pkg =
  let key = OpamPackage.to_string pkg in
  match Hashtbl.find_opt t.hash_cache key with
  | Some h -> h
  | None ->
      let patches = patches_for t pkg in
      let h =
        match patches with
        | [] -> ""
        | ps ->
            let contents =
              List.map
                (fun p ->
                  match Bos.OS.File.read (Fpath.v p) with
                  | Ok s -> s
                  | Error _ -> "")
                ps
            in
            Digest.string (String.concat "\n" contents) |> Digest.to_hex
      in
      Hashtbl.replace t.hash_cache key h;
      h

let has_patches t pkg = patches_for t pkg <> []

let patch_args t pkg =
  List.mapi
    (fun i _p -> Printf.sprintf "--patch /patches/%03d.patch" i)
    (patches_for t pkg)
  |> String.concat " "

let patch_filenames t pkg = List.map Filename.basename (patches_for t pkg)
