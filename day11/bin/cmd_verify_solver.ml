(** verify-solver: differential test of incremental solution reuse.

    Walks a series of opam-repository commits (oldest first). At each commit it
    solves every target twice:

    - {b full}: from scratch, no reuse — the reference arm;
    - {b incr}: seeded from the previous commit's incr solutions via
      {!Day11_batch.Incremental_solver.reuse_solutions} with the same diff +
      [expected_cache_key]/[rekey_to] discipline the ocaml-docs-ci solver op
      uses, then solving only what wasn't reused.

    Every target's [Solve_result] must then be identical across the two arms
    (JSON equality — packages, build_deps, doc_deps {e and} examined). At the
    first commit both arms solve from scratch, which doubles as a
    solver-determinism baseline: if full-vs-full already differs, incremental
    reuse can't be validated.

    Exit code is non-zero if any mismatch was found. *)

open Cmdliner
module Inc = Day11_batch.Incremental_solver

let time f =
  let t0 = Unix.gettimeofday () in
  let r = f () in
  (r, Unix.gettimeofday () -. t0)

(* Last [count] first-parent commits of [head] (or HEAD), oldest
   first. Shells out to git: the CLI already requires a git checkout
   and this avoids hand-walking the commit graph. *)
let commits_of_repo ~repo ~head ~count =
  let cmd =
    Printf.sprintf "git -C %s log --first-parent -n %d --format=%%H %s"
      (Filename.quote repo) count (Filename.quote head)
  in
  let ic = Unix.open_process_in cmd in
  let commits = ref [] in
  (try
     while true do
       commits := input_line ic :: !commits
     done
   with End_of_file -> ());
  (match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ()
  | _ -> failwith ("git log failed: " ^ cmd));
  !commits (* git prints newest first; the fold reversed it *)

let short sha = String.sub sha 0 (min 12 (String.length sha))

(* Latest non-avoided version of [name], mirroring the batch CLI's
   latest-1 target selection. *)
let latest_version git_packages name =
  let versions = Day11_opam.Git_packages.get_versions git_packages name in
  let non_avoided =
    OpamPackage.Version.Map.filter
      (fun _v opam -> not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
      versions
  in
  let versions =
    if OpamPackage.Version.Map.is_empty non_avoided then versions
    else non_avoided
  in
  match OpamPackage.Version.Map.max_binding_opt versions with
  | Some (v, _) -> Some (OpamPackage.create name v)
  | None -> None

let targets_at git_packages ~only =
  let names =
    match only with
    | [] -> Day11_opam.Git_packages.all_names git_packages
    | names -> List.map OpamPackage.Name.of_string names
  in
  List.filter_map (latest_version git_packages) names
  |> List.sort OpamPackage.compare

(* Solve [targets] not already present in [dir] at [sha], saving each
   result (or failure) stamped with [cache_key]. Returns
   (solved, failed). *)
let solve_missing ~sw env ~np ~ocaml_version ~repo ~sha ~dir ~cache_key targets
    =
  ignore (Bos.OS.Dir.create ~path:true dir);
  let need =
    List.filter
      (fun t ->
        not
          (Sys.file_exists
             (Fpath.to_string Fpath.(dir / (OpamPackage.to_string t ^ ".json")))))
      targets
  in
  if need = [] then (0, 0)
  else
    let results =
      Day11_solver_pool.Solver_pool.solve_many ~sw env ?ocaml_version ~np
        ~repos:[ (repo, sha) ]
        need
    in
    let failed = ref 0 in
    List.iter
      (fun (target, result) ->
        let path = Fpath.(dir / (OpamPackage.to_string target ^ ".json")) in
        let entry =
          match result with
          | Ok result ->
              Inc.Cached_solution
                { package = target; result; cache_key = Some cache_key }
          | Error (error, examined) ->
              incr failed;
              Inc.Cached_failure
                {
                  package = target;
                  error;
                  examined;
                  cache_key = Some cache_key;
                }
        in
        ignore (Inc.save path entry))
      results;
    (List.length need, !failed)

type verdict =
  | Same
  | Both_failed
  | Result_differs of string (* which fields differ *)
  | Status_differs of string (* solved in one arm, failed in the other *)
  | Missing of string (* entry absent in one arm *)

let compare_target ~full_dir ~incr_dir target =
  let file d = Fpath.(d / (OpamPackage.to_string target ^ ".json")) in
  match (Inc.load (file full_dir), Inc.load (file incr_dir)) with
  | Error _, Error _ -> Missing "both arms"
  | Error _, Ok _ -> Missing "full arm"
  | Ok _, Error _ -> Missing "incr arm"
  | Ok (Inc.Cached_failure _), Ok (Inc.Cached_failure _) -> Both_failed
  | Ok (Inc.Cached_solution _), Ok (Inc.Cached_failure _) ->
      Status_differs "full solves, incr fails"
  | Ok (Inc.Cached_failure _), Ok (Inc.Cached_solution _) ->
      Status_differs "full fails, incr solves"
  | Ok (Inc.Cached_solution a), Ok (Inc.Cached_solution b) ->
      let ja = Day11_solution.Solve_result.to_json a.result in
      let jb = Day11_solution.Solve_result.to_json b.result in
      if ja = jb then Same
      else
        (* Classify which components differ, most interesting first. *)
        let module SR = Day11_solution.Solve_result in
        let fields =
          List.filter_map
            (fun (label, proj) ->
              if proj a.result <> proj b.result then Some label else None)
            [
              ( "packages",
                fun (r : SR.t) ->
                  `List
                    (List.map
                       (fun p -> `String (OpamPackage.to_string p))
                       (OpamPackage.Set.elements r.packages)) );
              ( "build_deps",
                fun (r : SR.t) ->
                  SR.to_json
                    {
                      r with
                      doc_deps = OpamPackage.Map.empty;
                      examined = OpamPackage.Name.Set.empty;
                    } );
              ( "doc_deps",
                fun (r : SR.t) ->
                  SR.to_json
                    {
                      r with
                      build_deps = OpamPackage.Map.empty;
                      examined = OpamPackage.Name.Set.empty;
                    } );
              ( "examined",
                fun (r : SR.t) ->
                  `List
                    (List.map
                       (fun n -> `String (OpamPackage.Name.to_string n))
                       (OpamPackage.Name.Set.elements r.examined)) );
            ]
        in
        Result_differs (String.concat "+" fields)

let key_for sha = "verify|" ^ sha

let run repo head count shas np ocaml_version_str only work_dir keep =
  let ocaml_version =
    if ocaml_version_str = "" then None
    else Some (OpamPackage.of_string ocaml_version_str)
  in
  let commits =
    match shas with [] -> commits_of_repo ~repo ~head ~count | l -> l
  in
  if List.length commits < 2 then (
    Printf.eprintf
      "Need at least 2 commits to verify incremental reuse (got %d).\n"
      (List.length commits);
    exit 2);
  let work_dir =
    match work_dir with
    | Some d -> Fpath.v d
    | None ->
        Fpath.v (Filename.get_temp_dir_name ()) |> fun t ->
        Fpath.(t / Printf.sprintf "day11-verify-%d" (Unix.getpid ()))
  in
  ignore (Bos.OS.Dir.create ~path:true work_dir);
  Printf.printf "=== verify-solver: %d commits, repo %s ===\n"
    (List.length commits) repo;
  Printf.printf "  work dir: %s\n%!" (Fpath.to_string work_dir);
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let store, _head = Day11_opam.Git_utils.get_git_repo_store_and_hash repo in
  let total_mismatch = ref 0 in
  let total_reused = ref 0 in
  let total_solved_incr = ref 0 in
  let total_solved_full = ref 0 in
  let prev : (string * Fpath.t) option ref = ref None in
  List.iteri
    (fun i sha ->
      let hash =
        Day11_opam.Git_utils.resolve_commit_in_store store (Some sha)
      in
      let git_packages = Day11_opam.Git_packages.of_commit store hash in
      let targets = targets_at git_packages ~only in
      let target_strs = List.map OpamPackage.to_string targets in
      Printf.printf "── commit %d/%d %s (%d targets) ──\n%!" (i + 1)
        (List.length commits) (short sha) (List.length targets);
      let full_dir = Fpath.(work_dir / "full" / short sha) in
      let incr_dir = Fpath.(work_dir / "incr" / short sha) in
      (* Full arm: always from scratch. *)
      let (n_full, f_full), t_full =
        time (fun () ->
            solve_missing ~sw env ~np ~ocaml_version ~repo ~sha ~dir:full_dir
              ~cache_key:(key_for sha) targets)
      in
      total_solved_full := !total_solved_full + n_full;
      (* Incr arm: seed from the previous commit's incr dir, then solve
       the remainder. At the first commit this is a from-scratch solve
       too — a solver-determinism baseline. *)
      let reused =
        match !prev with
        | None -> 0
        | Some (prev_sha, prev_incr_dir) ->
            let prev_hash =
              Day11_opam.Git_utils.resolve_commit_in_store store (Some prev_sha)
            in
            (* Union of both diff directions: the tree diff is asymmetric
           and the reverse direction catches added packages — same
           discipline as the ocaml-docs-ci wiring. *)
            let changed =
              let d1 =
                Day11_opam.Git_packages.diff_packages ~store prev_hash hash
              in
              let d2 =
                Day11_opam.Git_packages.diff_packages ~store hash prev_hash
              in
              List.fold_left
                (fun s n -> OpamPackage.Name.Set.add n s)
                OpamPackage.Name.Set.empty (d1 @ d2)
            in
            ignore (Bos.OS.Dir.create ~path:true incr_dir);
            let reused =
              Inc.reuse_solutions ~expected_cache_key:(key_for prev_sha)
                ~rekey_to:(key_for sha) ~solutions_cache_dir:incr_dir
                ~previous_dir:prev_incr_dir ~changed_packages:changed
                ~packages:target_strs ()
            in
            Printf.printf "  changed names: %d; reused %d/%d\n%!"
              (OpamPackage.Name.Set.cardinal changed)
              reused (List.length targets);
            reused
      in
      total_reused := !total_reused + reused;
      let (n_incr, f_incr), t_incr =
        time (fun () ->
            solve_missing ~sw env ~np ~ocaml_version ~repo ~sha ~dir:incr_dir
              ~cache_key:(key_for sha) targets)
      in
      total_solved_incr := !total_solved_incr + n_incr;
      Printf.printf
        "  solved: full=%d (%d failed, %.1fs)  incr=%d (%d failed, %.1fs)\n%!"
        n_full f_full t_full n_incr f_incr t_incr;
      (* Compare the arms. *)
      let same = ref 0 and both_failed = ref 0 and bad = ref [] in
      List.iter
        (fun target ->
          match compare_target ~full_dir ~incr_dir target with
          | Same -> incr same
          | Both_failed -> incr both_failed
          | Result_differs what ->
              bad := (target, "result differs: " ^ what) :: !bad
          | Status_differs what -> bad := (target, what) :: !bad
          | Missing what -> bad := (target, "missing in " ^ what) :: !bad)
        targets;
      total_mismatch := !total_mismatch + List.length !bad;
      Printf.printf "  compare: %d identical, %d both-failed, %d MISMATCH\n%!"
        !same !both_failed (List.length !bad);
      List.iteri
        (fun j (target, why) ->
          if j < 20 then
            Printf.printf "    MISMATCH %s: %s\n%!"
              (OpamPackage.to_string target)
              why)
        (List.rev !bad);
      if List.length !bad > 20 then
        Printf.printf "    … and %d more\n%!" (List.length !bad - 20);
      prev := Some (sha, incr_dir))
    commits;
  Printf.printf "=== summary ===\n";
  Printf.printf "  full solves: %d;  incr solves: %d;  reused: %d\n"
    !total_solved_full !total_solved_incr !total_reused;
  Printf.printf "  mismatches: %d\n%!" !total_mismatch;
  if not keep then ignore (Bos.OS.Dir.delete ~recurse:true work_dir)
  else Printf.printf "  solutions kept in %s\n%!" (Fpath.to_string work_dir);
  if !total_mismatch > 0 then 1 else 0

let repo_term =
  let doc = "Path to the opam-repository git checkout." in
  let default =
    match Sys.getenv_opt "HOME" with
    | Some h -> h ^ "/.day11/repo/opam-repository"
    | None -> "repo/opam-repository"
  in
  Arg.(value & opt string default & info [ "repo" ] ~docv:"PATH" ~doc)

let head_term =
  let doc = "Commit-ish to walk back from (default HEAD)." in
  Arg.(value & opt string "HEAD" & info [ "head" ] ~docv:"REF" ~doc)

let count_term =
  let doc =
    "Number of first-parent commits to verify (newest of these is $(b,--head); \
     processed oldest first)."
  in
  Arg.(value & opt int 5 & info [ "count"; "n" ] ~docv:"N" ~doc)

let shas_term =
  let doc =
    "Explicit commit SHAs to verify, oldest first (overrides \
     $(b,--count)/$(b,--head))."
  in
  Arg.(value & opt_all string [] & info [ "sha" ] ~docv:"SHA" ~doc)

let np_term =
  let doc = "Parallel solver workers." in
  Arg.(value & opt int 8 & info [ "np" ] ~docv:"N" ~doc)

let ocaml_version_term =
  let doc = "Pin the compiler (e.g. ocaml-base-compiler.5.2.1)." in
  Arg.(value & opt string "" & info [ "ocaml-version" ] ~docv:"PKG" ~doc)

let packages_term =
  let doc =
    "Restrict to these package names (repeatable). Default: every package name \
     at each commit, latest version."
  in
  Arg.(value & opt_all string [] & info [ "package"; "p" ] ~docv:"NAME" ~doc)

let work_dir_term =
  let doc =
    "Directory for the solution trees (default: a fresh dir under \\$TMPDIR, \
     deleted on success unless $(b,--keep))."
  in
  Arg.(value & opt (some string) None & info [ "work-dir" ] ~docv:"PATH" ~doc)

let keep_term =
  let doc = "Keep the solution trees for post-mortem." in
  Arg.(value & flag & info [ "keep" ] ~doc)

let cmd =
  let doc = "Differentially verify incremental solving against full solves" in
  let info =
    Cmd.info "verify-solver" ~doc
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Walks a series of opam-repository commits. At each commit every \
             target is solved twice: from scratch (the reference), and \
             incrementally (reusing the previous commit's solutions whose \
             examined set is untouched by the commits' changed packages — the \
             same machinery ocaml-docs-ci uses). The two results must be \
             identical for every target.";
          `P
            "The first commit solves from scratch in both arms, which doubles \
             as a solver-determinism baseline.";
          `P "Exits 1 if any mismatch is found.";
        ]
  in
  Cmd.v info
    Term.(
      const run
      $ repo_term
      $ head_term
      $ count_term
      $ shas_term
      $ np_term
      $ ocaml_version_term
      $ packages_term
      $ work_dir_term
      $ keep_term)
