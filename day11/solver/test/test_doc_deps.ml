(* Test that x-extra-doc-deps and {with-doc & post} produce the same solution.

   We register two fake packages in a real opam-repository:
   - test-old.1.0: uses x-extra-doc-deps
   - test-new.1.0: uses {with-doc & post}
   Both should solve to include the same set of packages. *)

let () =
  let opam_repo =
    match Sys.getenv_opt "OPAM_REPOSITORY" with
    | Some p -> p
    | None ->
        let home = Sys.getenv "HOME" in
        Filename.concat (Filename.concat home "ocaml") "opam-repository"
  in
  Printf.printf "Using opam-repository: %s\n%!" opam_repo;
  let git_packages, _ =
    Day11_opam.Git_packages.of_repositories [ (opam_repo, None) ]
  in
  Bos.OS.Dir.set_default_tmp (Fpath.v (Filename.get_temp_dir_name ()));
  let env =
    Day11_opam.Opam_env.std_env ~arch:"x86_64" ~os:"linux"
      ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()
  in

  (* Read both test opam files *)
  let read_opam path =
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path))
  in
  let opam_old = read_opam "/tmp/test-doc-deps/a-old.opam" in
  let opam_new = read_opam "/tmp/test-doc-deps/b-new.opam" in

  let ocaml_version =
    Some (OpamPackage.of_string "ocaml-base-compiler.5.4.1")
  in

  (* Create pins for each test package *)
  let make_pins name opam =
    OpamPackage.Name.Map.singleton
      (OpamPackage.Name.of_string name)
      (OpamPackage.Version.of_string "1.0", opam)
  in

  (* Solve test-old (x-extra-doc-deps) *)
  let target_old = OpamPackage.of_string "test-old.1.0" in
  let result_old =
    Day11_solver.Solve.solve ~packages:git_packages ~env
      ~pins:(make_pins "test-old" opam_old)
      ?ocaml_version target_old
  in

  (* Solve test-new ({with-doc & post}) *)
  let target_new = OpamPackage.of_string "test-new.1.0" in
  let result_new =
    Day11_solver.Solve.solve ~packages:git_packages ~env
      ~pins:(make_pins "test-new" opam_new)
      ?ocaml_version target_new
  in

  let pkg_names solution =
    OpamPackage.Map.fold
      (fun pkg _ acc -> OpamPackage.Name.Set.add (OpamPackage.name pkg) acc)
      solution OpamPackage.Name.Set.empty
  in

  match (result_old, result_new) with
  | Error (msg, _), _ ->
      Printf.printf "FAIL: test-old solve failed: %s\n%!" msg;
      exit 1
  | _, Error (msg, _) ->
      Printf.printf "FAIL: test-new solve failed: %s\n%!" msg;
      exit 1
  | Ok result_old, Ok result_new ->
      let sol_old = result_old.Day11_solution.Solve_result.build_deps in
      let sol_new = result_new.Day11_solution.Solve_result.build_deps in
      let names_old = pkg_names sol_old in
      let names_new = pkg_names sol_new in
      Printf.printf "test-old solution: %d packages\n%!"
        (OpamPackage.Map.cardinal sol_old);
      Printf.printf "test-new solution: %d packages\n%!"
        (OpamPackage.Map.cardinal sol_new);

      (* Remove the test package itself from each set *)
      let names_old =
        OpamPackage.Name.Set.remove
          (OpamPackage.Name.of_string "test-old")
          names_old
      in
      let names_new =
        OpamPackage.Name.Set.remove
          (OpamPackage.Name.of_string "test-new")
          names_new
      in

      let only_old = OpamPackage.Name.Set.diff names_old names_new in
      let only_new = OpamPackage.Name.Set.diff names_new names_old in

      if
        OpamPackage.Name.Set.is_empty only_old
        && OpamPackage.Name.Set.is_empty only_new
      then Printf.printf "PASS: both solve to the same packages\n%!"
      else (
        if not (OpamPackage.Name.Set.is_empty only_old) then (
          Printf.printf "Only in old: ";
          OpamPackage.Name.Set.iter
            (fun n -> Printf.printf "%s " (OpamPackage.Name.to_string n))
            only_old;
          Printf.printf "\n%!");
        if not (OpamPackage.Name.Set.is_empty only_new) then (
          Printf.printf "Only in new: ";
          OpamPackage.Name.Set.iter
            (fun n -> Printf.printf "%s " (OpamPackage.Name.to_string n))
            only_new;
          Printf.printf "\n%!");
        Printf.printf "FAIL: solutions differ\n%!";
        exit 1)
