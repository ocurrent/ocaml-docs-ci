(** diff command: compare two snapshots within a profile *)

open Cmdliner

(* Load all solutions from a snapshot dir, returning (pkg, solve_result) list *)
let load_solutions snap_dir =
  let sol_dir = Day11_batch.Snapshot.solutions_dir snap_dir in
  match Bos.OS.Dir.contents sol_dir with
  | Error _ -> []
  | Ok files ->
      List.filter_map
        (fun path ->
          if not (Fpath.has_ext ".json" path) then None
          else if Fpath.basename path = "repos.json" then None
          else
            match Day11_batch.Incremental_solver.load path with
            | Ok
                (Day11_batch.Incremental_solver.Cached_solution
                   { package; result; _ }) ->
                Some (package, Some result)
            | Ok (Day11_batch.Incremental_solver.Cached_failure { package; _ })
              ->
                Some (package, None)
            | Error _ -> None)
        files

(* Extract package -> version map from solutions *)
let version_map solutions =
  List.fold_left
    (fun acc (pkg, result) ->
      let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
      let ver = OpamPackage.Version.to_string (OpamPackage.version pkg) in
      let solved = result <> None in
      let deps =
        match result with
        | Some r ->
            OpamPackage.Map.cardinal r.Day11_solution.Solve_result.build_deps
        | None -> 0
      in
      (name, (ver, solved, deps)) :: acc)
    [] solutions

let run profile_name profile_dir snap1_key snap2_key =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) ->
      let all_snaps = Common.snapshot_dirs_by_recency paths in
      if List.length all_snaps < 2 && snap1_key = None then (
        Printf.printf "Need at least 2 snapshots to diff (have %d)\n%!"
          (List.length all_snaps);
        1)
      else
        (* Resolve snapshot dirs *)
        let find_snap key =
          List.find_opt (fun p -> String.equal (Fpath.basename p) key) all_snaps
        in
        let snap1_dir, snap2_dir =
          match (snap1_key, snap2_key) with
          | Some k1, Some k2 -> (
              match (find_snap k1, find_snap k2) with
              | Some d1, Some d2 -> (d1, d2)
              | None, _ ->
                  Printf.eprintf "Snapshot %s not found\n" k1;
                  exit 1
              | _, None ->
                  Printf.eprintf "Snapshot %s not found\n" k2;
                  exit 1)
          | _ -> (
              (* Default: compare two most recent *)
              match all_snaps with
              | d1 :: d2 :: _ -> (d2, d1) (* older first *)
              | _ ->
                  Printf.eprintf "Not enough snapshots\n";
                  exit 1)
        in
        let key1 = Fpath.basename snap1_dir in
        let key2 = Fpath.basename snap2_dir in
        (* Load snapshot metadata *)
        let snap1 = Day11_batch.Snapshot.load snap1_dir in
        let snap2 = Day11_batch.Snapshot.load snap2_dir in
        Printf.printf "Comparing snapshots:\n";
        Printf.printf "  Old: %s" key1;
        (match snap1 with Ok s -> Printf.printf " (%s)" s.created | _ -> ());
        Printf.printf "\n";
        Printf.printf "  New: %s" key2;
        (match snap2 with Ok s -> Printf.printf " (%s)" s.created | _ -> ());
        Printf.printf "\n\n";
        (* Load solutions *)
        let sols1 = version_map (load_solutions snap1_dir) in
        let sols2 = version_map (load_solutions snap2_dir) in
        let tbl1 = Hashtbl.create (List.length sols1) in
        List.iter (fun (name, v) -> Hashtbl.replace tbl1 name v) sols1;
        let tbl2 = Hashtbl.create (List.length sols2) in
        List.iter (fun (name, v) -> Hashtbl.replace tbl2 name v) sols2;
        (* Collect all package names *)
        let all_names = Hashtbl.create 64 in
        Hashtbl.iter (fun k _ -> Hashtbl.replace all_names k ()) tbl1;
        Hashtbl.iter (fun k _ -> Hashtbl.replace all_names k ()) tbl2;
        let names =
          Hashtbl.fold (fun k () acc -> k :: acc) all_names []
          |> List.sort String.compare
        in
        (* Categorize changes *)
        let added = ref [] in
        let removed = ref [] in
        let version_changed = ref [] in
        let solve_changed = ref [] in
        let unchanged = ref 0 in
        List.iter
          (fun name ->
            match (Hashtbl.find_opt tbl1 name, Hashtbl.find_opt tbl2 name) with
            | None, Some (ver, solved, _) ->
                added := (name, ver, solved) :: !added
            | Some (ver, solved, _), None ->
                removed := (name, ver, solved) :: !removed
            | Some (v1, s1, _), Some (v2, s2, _) ->
                if v1 <> v2 then
                  version_changed := (name, v1, v2) :: !version_changed
                else if s1 <> s2 then
                  solve_changed := (name, s1, s2) :: !solve_changed
                else incr unchanged
            | None, None -> ())
          names;
        (* Print results *)
        if !version_changed <> [] then (
          Printf.printf "Version changes:\n";
          List.iter
            (fun (name, v1, v2) -> Printf.printf "  %s: %s -> %s\n" name v1 v2)
            (List.rev !version_changed);
          Printf.printf "\n");
        if !added <> [] then (
          Printf.printf "Added:\n";
          List.iter
            (fun (name, ver, solved) ->
              Printf.printf "  %s.%s%s\n" name ver
                (if solved then "" else " (solve failed)"))
            (List.rev !added);
          Printf.printf "\n");
        if !removed <> [] then (
          Printf.printf "Removed:\n";
          List.iter
            (fun (name, ver, solved) ->
              Printf.printf "  %s.%s%s\n" name ver
                (if solved then "" else " (was failing)"))
            (List.rev !removed);
          Printf.printf "\n");
        if !solve_changed <> [] then (
          Printf.printf "Solve status changed:\n";
          List.iter
            (fun (name, was_ok, now_ok) ->
              let status = if now_ok then "now solves" else "now fails" in
              ignore was_ok;
              Printf.printf "  %s: %s\n" name status)
            (List.rev !solve_changed);
          Printf.printf "\n");
        Printf.printf
          "Summary: %d unchanged, %d version changes, %d added, %d removed, %d \
           solve changes\n"
          !unchanged
          (List.length !version_changed)
          (List.length !added) (List.length !removed)
          (List.length !solve_changed);
        0

let snap1_term =
  let doc = "First snapshot key (default: second most recent)" in
  Arg.(value & opt (some string) None & info [ "from" ] ~docv:"KEY" ~doc)

let snap2_term =
  let doc = "Second snapshot key (default: most recent)" in
  Arg.(value & opt (some string) None & info [ "to" ] ~docv:"KEY" ~doc)

let cmd =
  let doc = "Compare two snapshots within a profile" in
  let info = Cmd.info "diff" ~doc in
  Cmd.v info
    Term.(
      const run
      $ Common.profile_term
      $ Common.profile_dir_term
      $ snap1_term
      $ snap2_term)
