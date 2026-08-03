(* Integration test: doc dependency analysis and odoc tool building.

   Requires: the from-scratch build cache at /tmp/day11-scratch-cache,
   opam-repository at /home/jjl25/opam-repository
   Run with: DAY11_INTEGRATION=true dune exec day11/doc/test/test_doc_integration.exe *)

open Day11_doc
open Day11_test_util.Test_util

let cache_dir = Fpath.v "/tmp/day11-scratch-cache"
let os_dir = Fpath.(cache_dir / "linux-x86_64")
let packages_dir = Fpath.(os_dir / "packages")
let base_dir = Fpath.(cache_dir / "base")
let switch = "default"

let make_base () : Day11_layer.Base.t =
  {
    hash =
      Day11_opam_build.Base.build_hash ~os_distribution:"debian"
        ~os_version:"bookworm" ~arch:"x86_64" ();
    dir = base_dir;
    image = "debian:bookworm";
  }

(** Find a build layer for a package *)
let find_layer_for env pkg_str =
  let symlinks =
    Day11_layer.Scan.list_package_symlinks ~exclude:[] env packages_dir pkg_str
  in
  match
    List.find_opt
      (fun (name, _) -> Astring.String.is_prefix ~affix:"build-" name)
      symlinks
  with
  | Some (name, _) -> Some name
  | None -> None

(* ── Prep test ───────────────────────────────────────────────────── *)

let test_prep_from_real_layer () =
  with_eio @@ fun ~sw:_ env ->
  let astring_layer_name =
    match find_layer_for env "astring.0.8.5" with
    | Some name -> name
    | None -> Alcotest.skip ()
  in
  let astring_layer = Fpath.(os_dir / astring_layer_name) in
  let installed_libs =
    Day11_opam_layer.Installed_files.scan_libs ~layer_dir:astring_layer
  in
  let installed_docs =
    Day11_opam_layer.Installed_files.scan_docs ~layer_dir:astring_layer
  in
  let dest_dir = Bos.OS.Dir.tmp "day11_doc_prep_%s" |> Result.get_ok in
  Fun.protect
    ~finally:(fun () -> Bos.OS.Dir.delete ~recurse:true dest_dir |> ignore)
    (fun () ->
      let prep_root, _mounts =
        Prep.create_with_mounts ~source_layer_dir:astring_layer
          ~dest_layer_dir:dest_dir ~universe:"test-universe"
          ~pkg:(OpamPackage.of_string "astring.0.8.5")
          ~installed_libs ~installed_docs
        |> ok_or_fail "prep"
      in
      let prep_lib =
        Fpath.(
          prep_root
          / "universes"
          / "test-universe"
          / "astring"
          / "0.8.5"
          / "lib")
      in
      Alcotest.(check bool)
        "prep dir exists" true
        (Bos.OS.Dir.exists prep_lib |> Result.get_ok))

let test_command_generation () =
  let cmd =
    Command.odoc_driver_voodoo
      ~pkg:(OpamPackage.of_string "astring.0.8.5")
      ~universe:"abc123" ~blessed:true
      ~actions:(Phase.phase_to_string Phase.Doc_all)
      ~odoc_bin:"/home/opam/.opam/default/bin/odoc"
      ~odoc_md_bin:"/home/opam/.opam/default/bin/odoc-md"
  in
  Alcotest.(check bool)
    "has astring" true
    (Astring.String.is_infix ~affix:"astring" cmd)

(* ── Build odoc ──────────────────────────────────────────────────── *)

let test_build_odoc () =
  with_eio @@ fun ~sw env ->
  if not (Bos.OS.Dir.exists base_dir |> Result.get_ok) then Alcotest.skip ();
  let opam_repository = opam_repository () in
  let base = make_base () in
  let git_packages, repos_with_shas =
    Day11_opam.Git_packages.of_repositories [ (opam_repository, None) ]
  in
  let odoc_versions =
    Day11_opam.Git_packages.get_versions git_packages
      (OpamPackage.Name.of_string "odoc")
  in
  let odoc_pkg =
    match OpamPackage.Version.Map.max_binding_opt odoc_versions with
    | Some (v, _) -> OpamPackage.create (OpamPackage.Name.of_string "odoc") v
    | None -> Alcotest.skip ()
  in
  Printf.printf "Building %s...\n%!" (OpamPackage.to_string odoc_pkg);
  let benv : Day11_opam_build.Types.build_env =
    { base; os_dir; uid = 1000; gid = 1000; cpu_slots = None }
  in
  let tool =
    Day11_opam_build.Tools.build_tool ~sw env benv ~packages:git_packages
      ~repos:repos_with_shas odoc_pkg
    |> ok_or_fail "build_tool"
  in
  Printf.printf "odoc built: %d layers\n%!" (List.length tool.builds);
  let _ =
    Day11_sys.Sudo.run ~sw env
      Bos.Cmd.(
        v "chmod" % "-R" % "a+rX" % Fpath.to_string Fpath.(tool.dir / "fs"))
  in
  let odoc_bin =
    Fpath.(
      tool.dir / "fs" / "home" / "opam" / ".opam" / switch / "bin" / "odoc")
  in
  Alcotest.(check bool)
    "odoc binary built" true
    (Bos.OS.File.exists odoc_bin |> Result.get_ok)

(* ── Registration ────────────────────────────────────────────────── *)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_doc_integration"
      [
        ( "Prep",
          [
            Alcotest.test_case "from real layer" `Quick
              test_prep_from_real_layer;
          ] );
        ( "Command",
          [ Alcotest.test_case "generation" `Quick test_command_generation ] );
        ("Doc_build", [ Alcotest.test_case "build odoc" `Slow test_build_odoc ]);
      ]
