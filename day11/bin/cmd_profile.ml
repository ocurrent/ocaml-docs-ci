(** profile command: create, show, list, delete profiles *)

open Cmdliner

let profile_dir_term =
  let doc = "Profile directory (default ~/.day11)" in
  Arg.(value & opt string "" & info [ "profile-dir" ] ~docv:"DIR" ~doc)

let resolve_profile_dir s =
  if s = "" then Day11_batch.Profile.default_dir () else Fpath.v s

let name_term =
  let doc = "Profile name" in
  Arg.(required & opt (some string) None & info [ "name" ] ~docv:"NAME" ~doc)

(* ── create ────────────────────────────────────────────────────── *)

let opam_repo_term =
  let doc =
    "opam-repository path (repeatable, layered in order). Stored verbatim in \
     the profile; a relative path is taken relative to the .day11 root (the \
     profile dir's parent) when the profile is later loaded, so it needn't be \
     absolute."
  in
  Arg.(
    non_empty & opt_all string [] & info [ "opam-repository" ] ~docv:"DIR" ~doc)

let odoc_repo_term =
  let doc = "Local odoc checkout to pin" in
  Arg.(value & opt (some string) None & info [ "odoc-repo" ] ~docv:"DIR" ~doc)

let opam_build_repo_term =
  let doc = "Local opam-build checkout" in
  Arg.(
    value & opt (some string) None & info [ "opam-build-repo" ] ~docv:"DIR" ~doc)

let compiler_term =
  let doc = "Compiler version constraint (e.g. ocaml-base-compiler.5.4.1)" in
  Arg.(value & opt (some string) None & info [ "compiler" ] ~docv:"PKG" ~doc)

let all_versions_term =
  let doc = "Build all versions of each package" in
  Arg.(value & flag & info [ "all-versions" ] ~doc)

let small_universe_term =
  let doc = "Build a curated small universe" in
  Arg.(value & flag & info [ "small-universe" ] ~doc)

let with_doc_term =
  let doc = "Generate documentation" in
  Arg.(value & flag & info [ "with-doc" ] ~doc)

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

let driver_compiler_term =
  let doc =
    "Compiler for doc driver tools (default: auto-detect from solutions)"
  in
  Arg.(value & opt string "" & info [ "driver-compiler" ] ~docv:"PKG" ~doc)

let run_create profile_dir name opam_repositories odoc_repo opam_build_repo
    compiler all_versions small_universe with_doc arch os_distribution
    os_version driver_compiler =
  let dir = Fpath.(resolve_profile_dir profile_dir / "profiles") in
  let target_mode : Day11_batch.Profile.target_mode =
    if small_universe then
      {
        versions = Day11_batch.Profile.Latest_n 1;
        names = Day11_batch.Profile.Names Day11_batch.Targets.small_universe;
      }
    else if all_versions then
      {
        versions = Day11_batch.Profile.All_versions;
        names = Day11_batch.Profile.All_names;
      }
    else
      {
        versions = Day11_batch.Profile.All_versions;
        names = Day11_batch.Profile.All_names;
      }
  in
  let profile : Day11_batch.Profile.t =
    {
      name;
      opam_repositories;
      odoc_repo;
      opam_build_repo;
      compiler;
      target_mode;
      with_doc;
      with_jtw = false;
      jtw_repo = None;
      arch;
      os_distribution;
      os_version;
      driver_compiler;
      extra_pins = [];
      pinned_versions = [];
      patches_dir = None;
      base_image_digest = None;
      base_image_updated = None;
      html_dir = None;
    }
  in
  match Day11_batch.Profile.save ~dir profile with
  | Ok () ->
      Printf.printf "Profile '%s' created.\n%!" name;
      Fmt.pr "%a@." Day11_batch.Profile.pp profile;
      0
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1

let create_cmd =
  let doc = "Create a new profile" in
  let info = Cmd.info "create" ~doc in
  Cmd.v info
    Term.(
      const run_create
      $ profile_dir_term
      $ name_term
      $ opam_repo_term
      $ odoc_repo_term
      $ opam_build_repo_term
      $ compiler_term
      $ all_versions_term
      $ small_universe_term
      $ with_doc_term
      $ arch_term
      $ os_distribution_term
      $ os_version_term
      $ driver_compiler_term)

(* ── show ──────────────────────────────────────────────────────── *)

let run_show profile_dir name =
  let dir = Fpath.(resolve_profile_dir profile_dir / "profiles") in
  match Day11_batch.Profile.load ~dir ~name with
  | Ok profile ->
      Fmt.pr "%a@." Day11_batch.Profile.pp profile;
      0
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1

let show_cmd =
  let doc = "Show a profile" in
  let info = Cmd.info "show" ~doc in
  Cmd.v info Term.(const run_show $ profile_dir_term $ name_term)

(* ── list ──────────────────────────────────────────────────────── *)

let run_list profile_dir =
  let dir = Fpath.(resolve_profile_dir profile_dir / "profiles") in
  let names = Day11_batch.Profile.list ~dir in
  if names = [] then
    Printf.printf "No profiles found in %s\n%!" (Fpath.to_string dir)
  else List.iter (fun name -> Printf.printf "%s\n" name) names;
  0

let list_cmd =
  let doc = "List profiles" in
  let info = Cmd.info "list" ~doc in
  Cmd.v info Term.(const run_list $ profile_dir_term)

(* ── delete ────────────────────────────────────────────────────── *)

let run_delete profile_dir name =
  let dir = Fpath.(resolve_profile_dir profile_dir / "profiles") in
  match Day11_batch.Profile.delete ~dir ~name with
  | Ok () ->
      Printf.printf "Profile '%s' deleted.\n%!" name;
      0
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1

let delete_cmd =
  let doc = "Delete a profile" in
  let info = Cmd.info "delete" ~doc in
  Cmd.v info Term.(const run_delete $ profile_dir_term $ name_term)

(* ── refresh-base ──────────────────────────────────────────────── *)

let run_refresh_base profile_dir name =
  let dir = Fpath.(resolve_profile_dir profile_dir / "profiles") in
  match Day11_batch.Profile.load ~dir ~name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok profile -> (
      Printf.printf "Resolving digest for %s (this may take ~15s)...\n%!"
        (Day11_batch.Profile.base_image_tag profile);
      match Day11_batch.Profile.refresh_base_digest profile with
      | Error (`Msg e) ->
          Printf.eprintf "Error: %s\n%!" e;
          1
      | Ok updated -> (
          match Day11_batch.Profile.save ~dir updated with
          | Ok () ->
              Printf.printf "Base image digest updated:\n  %s\n%!"
                (Option.value ~default:"?" updated.base_image_digest);
              0
          | Error (`Msg e) ->
              Printf.eprintf "Error saving: %s\n%!" e;
              1))

let refresh_base_cmd =
  let doc = "Resolve and pin the base Docker image digest from the registry" in
  let info = Cmd.info "refresh-base" ~doc in
  Cmd.v info Term.(const run_refresh_base $ profile_dir_term $ name_term)

(* ── group ─────────────────────────────────────────────────────── *)

let cmd =
  let doc = "Manage analysis profiles" in
  let info = Cmd.info "profile" ~doc in
  Cmd.group info
    [ create_cmd; show_cmd; list_cmd; delete_cmd; refresh_base_cmd ]
