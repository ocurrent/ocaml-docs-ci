(* Integration test: build an OCaml package using Build_layer.build.

   Requires: Linux, runc, sudo, Docker, network access.
   Run with: DAY11_INTEGRATION=true dune exec day11/opam_build/test/test_build_integration.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let base_image = "ocaml/opam:debian-ocaml-5.2"

let test_build_astring () = with_eio @@ fun ~sw env ->
  let cache_dir = Fpath.v "/tmp/day11-build-cache" in
  mkdir cache_dir;
  let os_dir = Fpath.(cache_dir / "linux-x86_64") in
  mkdir os_dir;
  let base = Base.ensure ~sw env ~os_dir ~image:base_image
    |> ok_or_fail "base" in
  Printf.printf "Base: %s\n%!" (Fpath.to_string base.dir);
  let benv : Types.build_env =
    { base; os_dir; uid = 1000; gid = 1000; cpu_slots = None } in
  let pkg = OpamPackage.of_string "astring.0.8.5" in
  let layer_hash = Day11_layer.Hash.of_strings
    [ "build"; base.hash; "astring.0.8.5" ] in
  let node : Day11_opam_layer.Build.t =
    { hash = layer_hash; pkg; deps = []; universe = Day11_solution.Universe.dummy } in
  let result =
    Build_layer.build ~sw env benv
      ~opam_repositories:[]
      node
      ~strategy:{ cmd = Printf.sprintf "opam install -y %s"
                    (OpamPackage.to_string pkg);
                  cleanup = fun ~sw:_ _ _ -> () }
      ()
  in
  (match result with
   | Types.Success bl ->
       Printf.printf "SUCCESS: %s\n%!" bl.hash;
       let installed = Day11_opam_layer.Installed_files.scan_libs
         ~layer_dir:(Day11_opam_layer.Build.dir ~os_dir:benv.os_dir bl) in
       Printf.printf "Installed: %d lib files\n%!" (List.length installed);
       Alcotest.(check bool) "has astring"
         true (List.exists (fun f ->
           Astring.String.is_prefix ~affix:"astring" f) installed)
   | Types.Failure name ->
       Alcotest.fail (Printf.sprintf "Build failed: %s" name)
   | _ ->
       Alcotest.fail "Unexpected build result")

let test_build_cached () = with_eio @@ fun ~sw env ->
  let cache_dir = Fpath.v "/tmp/day11-build-cache" in
  let os_dir = Fpath.(cache_dir / "linux-x86_64") in
  let base = Base.ensure ~sw env ~os_dir ~image:base_image
    |> ok_or_fail "base" in
  let benv : Types.build_env =
    { base; os_dir; uid = 1000; gid = 1000; cpu_slots = None } in
  let pkg = OpamPackage.of_string "astring.0.8.5" in
  let layer_hash = Day11_layer.Hash.of_strings
    [ "build"; base.hash; "astring.0.8.5" ] in
  let node : Day11_opam_layer.Build.t =
    { hash = layer_hash; pkg; deps = []; universe = Day11_solution.Universe.dummy } in
  let t0 = Unix.gettimeofday () in
  let result =
    Build_layer.build ~sw env benv
      ~opam_repositories:[]
      node
      ~strategy:{ cmd = Printf.sprintf "opam install -y %s"
                    (OpamPackage.to_string pkg);
                  cleanup = fun ~sw:_ _ _ -> () }
      ()
  in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "Cache hit in %.3fs\n%!" elapsed;
  Alcotest.(check bool) "fast cache hit" true (elapsed < 1.0);
  match result with
  | Types.Success _ -> ()
  | _ -> Alcotest.fail "Expected cache hit Success"

let () =
  if not (is_integration ()) then
    Printf.printf
      "Skipping integration tests (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_build_integration"
      [
        ( "Build_layer",
          [
            Alcotest.test_case "build astring" `Slow test_build_astring;
            Alcotest.test_case "cache hit" `Quick test_build_cached;
          ] );
      ]
