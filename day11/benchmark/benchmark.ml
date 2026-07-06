(* Benchmark: compare day11 operations with timing *)

let opam_repository =
  try Sys.getenv "OPAM_REPOSITORY"
  with Not_found -> "/home/jjl25/opam-repository"

let time name f =
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "%-40s %.3fs\n%!" name elapsed;
  result

let () =
  Printf.printf "=== day11 benchmark ===\n\n";

  (* 1. Solver setup *)
  let git_packages, repos_with_shas =
    time "Load opam-repository (git)" (fun () ->
      Day11_opam.Git_packages.of_repositories
        [ (opam_repository, None) ]) in
  let opam_env = Day11_opam.Opam_env.std_env
    ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
    ~os_family:"debian" ~os_version:"12" () in

  (* 2. Single solve *)
  let _sol_astring =
    time "Solve astring.0.8.5" (fun () ->
      Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
        (OpamPackage.of_string "astring.0.8.5")) in

  let _sol_odoc =
    time "Solve odoc.3.1.0" (fun () ->
      Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
        (OpamPackage.of_string "odoc.3.1.0")) in

  let _sol_base =
    time "Solve base.v0.17.3" (fun () ->
      Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
        (OpamPackage.of_string "base.v0.17.3")) in

  (* 3. Solve 50 packages *)
  let packages_50 = [
    "astring.0.8.5"; "fmt.0.9.0"; "bos.0.2.1"; "logs.0.7.0";
    "cmdliner.1.3.0"; "yojson.2.2.2"; "ptime.1.2.0"; "uutf.1.0.3";
    "re.1.14.0"; "cstruct.6.2.0"; "lwt.6.0.0"; "eio.1.3";
    "ppxlib.0.37.0"; "menhir.20260209"; "sedlex.3.7";
    "odoc.3.1.0"; "base.v0.17.3"; "core.v0.17.1";
    "zarith.1.14"; "num.1.6"; "ocamlgraph.2.2.0";
    "js_of_ocaml.6.3.2"; "brr.0.0.8"; "tyxml.4.6.0";
    "jsonm.1.0.2"; "sexplib0.v0.17.0"; "parsexp.v0.17.0";
    "csexp.1.5.2"; "base64.3.5.2"; "bigstringaf.0.10.0";
    "cmarkit.0.4.0"; "fpath.0.7.3"; "rresult.0.7.0";
    "result.1.5"; "seq.base"; "topkg.1.1.1"; "ocamlbuild.0.16.1";
    "ocamlfind.1.9.8"; "cppo.1.8.0"; "gen.1.1"; "mtime.2.1.0";
    "progress.0.5.0"; "terminal.0.5.0"; "checkseum.0.5.2";
    "decompress.1.5.3"; "optint.0.3.0"; "hmap.0.8.1";
    "psq.0.2.1"; "angstrom.0.16.1"; "domain-local-await.1.0.1"
  ] in
  let solutions =
    time (Printf.sprintf "Solve %d packages" (List.length packages_50)) (fun () ->
      List.filter_map (fun pkg_str ->
        match Day11_solver.Solve.solve ~packages:git_packages ~env:opam_env
                (OpamPackage.of_string pkg_str) with
        | Ok result -> Some (OpamPackage.of_string pkg_str, result.Day11_solution.Solve_result.build_deps)
        | Error _ -> None
      ) packages_50) in
  Printf.printf "  → %d/%d solved\n%!" (List.length solutions) (List.length packages_50);

  (* 4. DAG construction *)
  let find_opam = Day11_opam.Git_packages.find_package git_packages in
  let cache = Day11_opam_build.Hash_cache.create ~find_opam () in
  let nodes =
    time "Build DAG (50 solutions)" (fun () ->
      Day11_opam_build.Dag.build_dag cache ~base_hash:"benchmark"
        (List.map (fun (t, d) -> (t, d, d)) solutions)) in
  Printf.printf "  → %d unique nodes\n%!" (List.length nodes);

  (* 5. Blessing *)
  let _blessing_maps =
    time "Compute blessings (50 solutions)" (fun () ->
      Day11_batch.Blessing.compute_blessings solutions) in

  (* 6. Build with warm cache (if available) *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let scratch_cache = Fpath.v "/tmp/day11-scratch-cache" in
  (match Day11_opam_build.Base.load_cached
    ~os_dir:Fpath.(scratch_cache / "linux-x86_64")
    ~os_distribution:"debian" ~os_version:"bookworm" with
  | None ->
    Printf.printf "\nNo cached base image — skipping build benchmarks\n%!"
  | Some base ->
    let os_dir = Fpath.(scratch_cache / "linux-x86_64") in
    let benv = Day11_opam_build.Types.make_build_env ~base ~os_dir
      ~uid:1000 ~gid:1000 () in
    Day11_opam_build.Types.ensure_dirs benv;
    (* Warm cache: build astring (should be instant) *)
    let astring_hash = Day11_opam_build.Hash_cache.layer_hash cache
      ~base_hash:base.hash [ OpamPackage.of_string "astring.0.8.5" ] in
    let astring_node : Day11_opam_layer.Build.t =
      { hash = astring_hash;
        pkg = OpamPackage.of_string "astring.0.8.5";
        deps = []; universe = Day11_solution.Universe.dummy } in
    ignore (time "Build astring (cache hit)" (fun () ->
      Day11_opam_build.Build_layer.build ~sw env benv ~opam_repositories:[] astring_node ()));
    (* Warm cache: build odoc-driver tool *)
    ignore (time "Tools.build_tool odoc-driver (cache hit)" (fun () ->
      Day11_opam_build.Tools.build_tool ~sw env benv
        ~packages:git_packages ~repos:repos_with_shas
        (OpamPackage.of_string "odoc-driver.3.1.0")));
  );
  Printf.printf "\nDone.\n%!"
