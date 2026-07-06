(* Integration test: build OCaml packages layer by layer.

   Requires: Linux, runc, sudo, Docker, network access.
   Run with: DAY11_INTEGRATION=true dune exec day11/opam_build/test/test_layered_build.exe *)

open Day11_opam_build
open Day11_test_util.Test_util

let base_image = "ocaml/opam:debian-ocaml-5.2"

let packages = [
  "ocamlbuild.0.16.1";
  "ocamlfind.1.9.8";
  "topkg.1.1.1";
  "astring.0.8.5";
]

let test_layered_build () = with_eio @@ fun ~sw env ->
  let cache_dir = Fpath.v "/tmp/day11-layered-cache" in
  mkdir cache_dir;
  let os_dir = Fpath.(cache_dir / "linux-x86_64") in
  mkdir os_dir;
  let base = Base.ensure ~sw env ~os_dir ~image:base_image
    |> ok_or_fail "base" in
  Printf.printf "Base: %s\n%!" (Fpath.to_string base.dir);
  let benv : Types.build_env =
    { base; os_dir; uid = 1000; gid = 1000; cpu_slots = None } in
  let _final =
    List.fold_left (fun (deps : Day11_opam_layer.Build.t list) pkg_str ->
      let pkg = OpamPackage.of_string pkg_str in
      let dep_hashes = List.map (fun (d : Day11_opam_layer.Build.t) -> d.hash) deps in
      let layer_hash = Day11_layer.Hash.of_strings
        ([ "build"; base.hash; pkg_str ] @ dep_hashes) in
      let node : Day11_opam_layer.Build.t =
        { hash = layer_hash; pkg; deps; universe = Day11_solution.Universe.dummy } in
      Printf.printf "\n--- Building %s (layer: %s, deps: %d) ---\n%!"
        pkg_str (Day11_opam_layer.Build.dir_name node) (List.length deps);
      let result =
        Build_layer.build ~sw env benv
          ~opam_repositories:[]
          node
          ~strategy:{ cmd = Printf.sprintf "opam install -y %s" pkg_str;
                      cleanup = fun ~sw:_ _ _ -> () }
          ()
      in
      match result with
      | Types.Success bl ->
          Printf.printf "OK: %s → %s\n%!" pkg_str bl.hash;
          let installed = Day11_opam_layer.Installed_files.scan_libs
            ~layer_dir:(Day11_opam_layer.Build.dir ~os_dir:benv.os_dir bl) in
          Printf.printf "  Installed: %d lib files\n%!" (List.length installed);
          deps @ [ bl ]
      | Types.Failure name ->
          Alcotest.fail (Printf.sprintf "%s failed: %s" pkg_str name)
      | _ ->
          Alcotest.fail (Printf.sprintf "%s unexpected" pkg_str)
    ) [] packages
  in
  Printf.printf "\n=== All %d packages built successfully ===\n%!"
    (List.length packages)

let () =
  if not (is_integration ()) then
    Printf.printf "Skipping (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_layered_build"
      [ ( "Layered",
          [ Alcotest.test_case "build astring deps" `Slow
              test_layered_build ] ) ]
