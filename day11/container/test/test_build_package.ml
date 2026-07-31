(* Integration test: import a Docker image as base layer, then build
   an OCaml package on top of it using overlay + runc.

   Requires: Linux, runc, sudo, Docker, network access (for opam).
   Run with: DAY11_INTEGRATION=true dune exec day11/container/test/test_build_package.exe *)

open Day11_test_util.Test_util

let base_image = "ocaml/opam:debian-ocaml-5.2"

(** Import a Docker image as the base layer, caching the result. *)
let ensure_base ~sw env ~cache_dir =
  let base_dir = Fpath.(cache_dir / "base") in
  let marker = Fpath.(base_dir / "fs" / "usr" / "bin" / "opam") in
  if Bos.OS.File.exists marker |> Result.get_ok then begin
    Printf.printf "Base layer cached at %s\n%!" (Fpath.to_string base_dir);
    base_dir
  end else begin
    Printf.printf "Importing %s...\n%!" base_image;
    Day11_layer.Import.from_docker ~sw env ~image:base_image ~layer_dir:base_dir
    |> ok_or_fail "import base";
    Printf.printf "Base layer imported to %s\n%!" (Fpath.to_string base_dir);
    base_dir
  end

(** Build a package by running opam install inside a container on top
    of the given layers. Returns the upper dir (new layer fs). *)
let build_package ~sw env ~cache_dir ~base_dir ~dep_layers ~pkg =
  let hash =
    Day11_layer.Hash.layer_hash
      ~base_hash:(Day11_layer.Hash.base_hash ~image:base_image)
      ~dep_hashes:(List.map Fpath.to_string dep_layers)
      ~pkg
  in
  let layer_dir = Fpath.(cache_dir / ("build-" ^ String.sub hash 0 12)) in
  let layer_json = Fpath.(layer_dir / "layer.json") in
  (* Check cache *)
  if Bos.OS.File.exists layer_json |> Result.get_ok then begin
    Printf.printf "Cache hit for %s (%s)\n%!" pkg (Fpath.to_string layer_dir);
    layer_dir
  end else begin
    Printf.printf "Building %s...\n%!" pkg;
    let temp_dir = Bos.OS.Dir.tmp "day11_build_%s" |> Result.get_ok in
    let lower = Fpath.(temp_dir / "lower") in
    let upper = Fpath.(temp_dir / "upper") in
    let work = Fpath.(temp_dir / "work") in
    let merged = Fpath.(temp_dir / "merged") in
    List.iter mkdir [ lower; upper; work; merged ];
    (* Stack base + dependency layers into lower *)
    Day11_layer.Stack.merge ~sw env
      ~layer_dirs:(base_dir :: dep_layers) ~target:lower
    |> ok_or_fail "stack";
    (* Mount overlay *)
    Day11_container.Overlay.mount ~sw env
      ~lower:[ lower ] ~upper ~work ~target:merged
    |> ok_or_fail "overlay mount";
    (* Figure out uid/gid from the base image's opam user *)
    let uid = 1000 and gid = 1000 in
    (* Generate OCI spec — run opam install with network for fetching *)
    let spec =
      Day11_container.Oci_spec.make
        ~cwd:"/home/opam"
        ~hostname:"builder"
        ~env:[
          ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
          ("HOME", "/home/opam");
          ("OPAMYES", "1");
          ("OPAMCONFIRMLEVEL", "unsafe-yes");
          ("OPAMERRLOGLEN", "0");
          ("OPAMPRECISETRACKING", "1");
        ]
        ~network:true
        ~argv:[ "/usr/bin/env"; "bash"; "-c";
                Printf.sprintf "opam install -y %s" pkg ]
        ~uid ~gid
        ()
    in
    Day11_container.Oci_spec.write
      ~root:(Fpath.to_string merged) temp_dir spec
    |> ok_or_fail "write spec";
    let container_id =
      Printf.sprintf "day11-build-%s-%d" pkg (Unix.getpid ())
    in
    ignore (Day11_container.Runc.delete ~sw env container_id);
    let run =
      Day11_container.Runc.run ~sw env ~bundle:temp_dir ~container_id
      |> ok_or_fail "runc run"
    in
    (* Always clean up *)
    ignore (Day11_container.Runc.delete ~sw env container_id);
    ignore (Day11_container.Overlay.umount ~sw env merged);
    let exit_code =
      match run.Day11_sys.Run.status with
      | `Exited n -> n
      | `Signaled n -> 128 + n
    in
    Printf.printf "Build %s: exit %d (%.1fs)\n%!" pkg exit_code run.time;
    if exit_code <> 0 then begin
      Printf.printf "STDOUT:\n%s\nSTDERR:\n%s\n" run.output run.errors;
      Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
      Alcotest.fail (Printf.sprintf "%s build failed (exit %d)" pkg exit_code)
    end;
    (* Move upper dir to layer_dir/fs *)
    mkdir layer_dir;
    let r = Day11_sys.Sudo.run  ~sw env
      Bos.Cmd.(v "mv" % Fpath.to_string upper
               % Fpath.to_string Fpath.(layer_dir / "fs")) in
    (match r with Ok _ -> () | Error (`Msg e) -> Alcotest.fail e);
    (* Write layer.json *)
    let meta : Day11_layer.Meta.t = {
      exit_status = exit_code;
      parent_hashes = [];
      uid = 1000; gid = 1000;
      base_hash = Day11_layer.Hash.base_hash ~image:base_image;
      disk_usage = 0;
      timing = Day11_layer.Meta.empty_timing;
      created_at = "";
      failed_dep = None;
    } in
    Day11_layer.Meta.save env
      Fpath.(layer_dir / "layer.json") meta
    |> ok_or_fail "save layer meta";
    (* Write build.json sidecar *)
    let bm : Day11_opam_layer.Build_meta.t = {
      package = pkg;
      deps = [];
      stack = [];
      build_deps = [];
      installed_libs = [];
      installed_docs = [];
      patches = [];
      base_image = "";
      cmd = "";
      universe = "";
    } in
    Day11_opam_layer.Build_meta.save layer_dir bm
    |> ok_or_fail "save build meta";
    (* Clean up temp dir *)
    Day11_sys.Sudo.rm_rf ~sw env temp_dir |> ignore;
    Printf.printf "Layer for %s at %s\n%!" pkg (Fpath.to_string layer_dir);
    layer_dir
  end

(* ── Tests ───────────────────────────────────────────────────────── *)

let test_build_astring () = with_eio @@ fun ~sw env ->
  let cache_dir = Fpath.v "/tmp/day11-integration-cache" in
  mkdir cache_dir;
  let base_dir = ensure_base ~sw env ~cache_dir in
  let astring_layer =
    build_package ~sw env ~cache_dir ~base_dir ~dep_layers:[] ~pkg:"astring"
  in
  (* Verify the layer has installed files.
     Note: layer fs/ is owned by the container's uid (1000), which may
     differ from our uid. Use sudo to list files for verification. *)
  let ls_run =
    Day11_sys.Run.run ~sw env
      Bos.Cmd.(v "sudo" % "find"
               % Fpath.to_string Fpath.(astring_layer / "fs" / "home" / "opam"
                                        / ".opam" / "5.2" / "lib" / "astring")
               % "-type" % "f" % "-name" % "*.cmi")
      None
  in
  let cmi_files = String.trim ls_run.output in
  Printf.printf "astring .cmi files:\n%s\n%!" cmi_files;
  Alcotest.(check bool) "has astring .cmi files"
    true (String.length cmi_files > 0);
  (* Also verify scan_libs works when we can read the files
     (fix permissions for non-sudo scanning) *)
  let _ = Day11_sys.Sudo.run  ~sw env
    Bos.Cmd.(v "chmod" % "-R" % "a+rX"
             % Fpath.to_string Fpath.(astring_layer / "fs")) in
  let installed = Day11_opam_layer.Installed_files.scan_libs
    ~layer_dir:astring_layer in
  Printf.printf "Installed lib files after chmod: %d\n%!" (List.length installed);
  List.iter (fun f -> Printf.printf "  %s\n%!" f)
    (List.filteri (fun i _ -> i < 10) installed);
  Alcotest.(check bool) "scan_libs finds astring"
    true (List.exists (fun f ->
      Astring.String.is_prefix ~affix:"astring" f) installed)

let () =
  if not (is_integration ()) then
    Printf.printf
      "Skipping build tests (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_build_package"
      [
        ( "Build",
          [
            Alcotest.test_case "astring" `Slow test_build_astring;
          ] );
      ]
