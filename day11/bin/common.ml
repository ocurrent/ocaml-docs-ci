(** Shared CLI terms and helpers *)

open Cmdliner

let os_dir_term =
  let doc = "OS-specific cache directory (e.g. /cache/linux-x86_64)" in
  Arg.(required & opt (some string) None & info [ "os-dir" ] ~docv:"DIR" ~doc)

let cache_dir_term =
  let doc = "Top-level cache directory" in
  Arg.(
    required & opt (some string) None & info [ "cache-dir" ] ~docv:"DIR" ~doc)

let opam_repo_term =
  let doc =
    "Path to opam-repository (repeatable, layered in order — later repos \
     override earlier ones)"
  in
  Arg.(
    non_empty & opt_all string [] & info [ "opam-repository" ] ~docv:"DIR" ~doc)

let np_term =
  let doc = "Number of parallel workers (default 4)" in
  Arg.(value & opt int 4 & info [ "np"; "j" ] ~docv:"N" ~doc)

let arch_term =
  let doc = "Architecture (default x86_64)" in
  Arg.(value & opt string "x86_64" & info [ "arch" ] ~docv:"ARCH" ~doc)

let os_distribution_term =
  let doc = "OS distribution (default debian)" in
  Arg.(
    value & opt string "debian" & info [ "os-distribution" ] ~docv:"DIST" ~doc)

let os_version_term =
  let doc = "OS version (default bookworm)" in
  Arg.(value & opt string "bookworm" & info [ "os-version" ] ~docv:"VER" ~doc)

let ocaml_version_term =
  let doc =
    "OCaml compiler version (e.g. ocaml-base-compiler.5.2.1 or \
     ocaml-variants.5.2.0+ox)"
  in
  Arg.(
    value & opt (some string) None & info [ "ocaml-version" ] ~docv:"PKG" ~doc)

let patches_dir_term =
  let doc =
    "Directory of patches to apply before building. Structure: \
     DIR/PKG.VERSION/*.patch"
  in
  Arg.(value & opt (some string) None & info [ "patches-dir" ] ~docv:"DIR" ~doc)

let opam_build_repo_term =
  let doc =
    "Path to local opam-build source checkout (builds opam-build from source \
     for the base image)"
  in
  Arg.(
    value & opt (some string) None & info [ "opam-build-repo" ] ~docv:"DIR" ~doc)

let with_eio f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f ~sw (env :> Eio_unix.Stdenv.base)

let fpath s = Fpath.v s

let setup_solver ?(arch = "x86_64") ?(os = "linux")
    ?(os_distribution = "debian") ?(os_family = "debian") ?(os_version = "12")
    opam_repositories =
  let repos_with_heads =
    List.map (fun repo -> (repo, None)) opam_repositories
  in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories repos_with_heads
  in
  (* ocaml-git corrupts Bos's temp dir setting — reset it *)
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  let opam_env =
    Day11_opam.Opam_env.std_env ~arch ~os ~os_distribution ~os_family
      ~os_version ()
  in
  (git_packages, repos_with_shas, opam_env)

let parse_ocaml_version = function
  | None -> None
  | Some s -> Some (OpamPackage.of_string s)

(* ── Profile support ───────────────────────────────────────────── *)

let profile_term =
  let doc = "Profile name (from ~/.day11/profiles/)" in
  Arg.(required & opt (some string) None & info [ "profile" ] ~docv:"NAME" ~doc)

let profile_dir_term =
  let doc = "Profile directory (default ~/.day11)" in
  Arg.(value & opt string "" & info [ "profile-dir" ] ~docv:"DIR" ~doc)

type paths = {
  profile_dir : Fpath.t;
  cache_dir : Fpath.t;
  os_dir : Fpath.t;
  snapshots_base : Fpath.t;
}

let resolve_profile_dir s =
  if s = "" then Day11_batch.Profile.default_dir () else Fpath.v s

let load_profile ~profile_dir ~name =
  let pdir = resolve_profile_dir profile_dir in
  let profiles_dir = Fpath.(pdir / "profiles") in
  match Day11_batch.Profile.load ~dir:profiles_dir ~name with
  | Error e -> Error e
  | Ok profile ->
      let cache_dir = Fpath.(pdir / "cache") in
      let os_dir =
        Fpath.(cache_dir / Day11_batch.Profile.os_dir_name profile)
      in
      let snapshots_base = Fpath.(pdir / "snapshots" / name) in
      Ok (profile, { profile_dir = pdir; cache_dir; os_dir; snapshots_base })

let ensure_paths (paths : paths) =
  ignore (Bos.OS.Dir.create ~path:true paths.cache_dir);
  ignore (Bos.OS.Dir.create ~path:true paths.os_dir);
  ignore (Bos.OS.Dir.create ~path:true paths.snapshots_base)

let latest_snapshot_dir (paths : paths) =
  match Bos.OS.Dir.contents paths.snapshots_base with
  | Error _ -> None
  | Ok entries -> (
      let dirs =
        entries
        |> List.filter (fun p -> Bos.OS.Dir.exists p |> Result.get_ok)
        |> List.filter_map (fun p ->
               try
                 let stat = Unix.stat (Fpath.to_string p) in
                 Some (p, stat.Unix.st_mtime)
               with Unix.Unix_error _ -> None)
        |> List.sort (fun (_, t1) (_, t2) -> compare t2 t1)
      in
      match dirs with (p, _) :: _ -> Some p | [] -> None)

let snapshot_dirs_by_recency (paths : paths) =
  match Bos.OS.Dir.contents paths.snapshots_base with
  | Error _ -> []
  | Ok entries ->
      entries
      |> List.filter (fun p -> Bos.OS.Dir.exists p |> Result.get_ok)
      |> List.filter_map (fun p ->
             try
               let stat = Unix.stat (Fpath.to_string p) in
               Some (p, stat.Unix.st_mtime)
             with Unix.Unix_error _ -> None)
      |> List.sort (fun (_, t1) (_, t2) -> compare t2 t1)
      |> List.map fst

let read_pins_from_dir dir =
  let opam_files =
    Sys.readdir dir
    |> Array.to_list
    |> List.filter (fun f -> Filename.check_suffix f ".opam")
  in
  List.fold_left
    (fun acc filename ->
      let name = Filename.chop_suffix filename ".opam" in
      let path = Filename.concat dir filename in
      try
        let opam = OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) in
        OpamPackage.Name.Map.add
          (OpamPackage.Name.of_string name)
          (OpamPackage.Version.of_string "dev", opam)
          acc
      with _ -> acc)
    OpamPackage.Name.Map.empty opam_files
