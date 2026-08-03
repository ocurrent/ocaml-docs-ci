(** Day11-based solver: in-process solving via solver_pool subprocesses.

    Replaces the Cap'n Proto solver service. Uses day11's solver_worker binaries
    for parallel solving, communicating via JSONL files. *)

(* Order-insensitive digest of a [(path, sha)] repo set. Part of both
   the OCurrent cache key and each per-target entry's [cache_key]. *)
let repos_digest repos =
  let sorted =
    List.sort compare (List.map (fun (path, sha) -> path ^ "@" ^ sha) repos)
  in
  Digest.to_hex (Digest.string (String.concat "\n" sorted))

module SolveOp = struct
  type t = {
    repos_with_shas : (string * string) list;
    env : Eio_unix.Stdenv.base;
    np : int;
    profile_name : string;
    ocaml_version : OpamPackage.t option;
    pinned_versions : OpamPackage.t list;
        (** Hard version pins fed into the solver as [(`Eq, v)] constraints.
            Empty list = no extra pins beyond [ocaml_version]. Surfaced via
            [Profile.pinned_versions]; used to propagate a specific +ox /
            variant flavour through transitive deps. *)
    cache_dir : Fpath.t;
        (** Used to derive the per-snapshot [solutions/] directory
            ([snapshot_dir/solutions/<pkg>.json]). The on-disk cache lets a
            pipeline restart after [sqlite.db] has been wiped (or after
            OCurrent's primitive cache otherwise loses state) skip the ~3-minute
            solver pass entirely as long as repos haven't moved and the compiler
            / target set is unchanged. *)
  }

  module Key = struct
    type t = {
      targets : OpamPackage.t list;
      commit : string;
      repos_digest : string;
      ocaml_version : string;
      (* String form — empty means unpinned. Including this in the
         cache key ensures a profile compiler change invalidates
         prior solves. *)
      pinned_versions : string list;
          (* Same shape as [ocaml_version] — string-form pins included
         in the digest so a profile pin change invalidates prior
         solves. *)
    }

    let digest t =
      t.commit
      ^ "@"
      ^ t.repos_digest
      ^ "|"
      ^ t.ocaml_version
      ^ "|"
      ^ String.concat "," t.pinned_versions
      ^ ":"
      ^ (List.map OpamPackage.to_string t.targets |> String.concat ",")
  end

  module Value = struct
    type t = {
      results : (string * string) list; (* (pkg_str, solve_result_json) pairs *)
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-solver"

  let pp f (key : Key.t) =
    Fmt.pf f "solve %d packages @%s" (List.length key.targets)
      (String.sub key.commit 0 (min 12 (String.length key.commit)))

  let auto_cancel = false

  let snapshot_dir_of ctx =
    Day11_profile_ctx_loader.snapshot_dir_of ~cache_dir:ctx.cache_dir
      ~profile_name:ctx.profile_name ctx.repos_with_shas

  let snapshot_solutions_dir ctx = Fpath.(snapshot_dir_of ctx / "solutions")

  (* [<...>/snapshots/<profile>] — the dir holding all of this
     profile's snapshot dirs. *)
  let profile_snapshots_base ctx = Fpath.parent (snapshot_dir_of ctx)

  (* Per-target solution cache.

     Files live at [snapshot_dir/solutions/<pkg>.<ver>.json] and are
     read/written via {!Day11_batch.Incremental_solver}. We embed a
     [cache_key = hash(compiler ∥ commit ∥ repos_digest)] in each
     entry, so within a snapshot a compiler or pin change invalidates
     prior entries. Because the key bakes in the global commit, a repo
     bump alone would invalidate {e everything} — that case is handled
     by the incremental-reuse pass below, which carries forward
     solutions provably unaffected by the commits' changed packages.

     This is deliberately per-file (not a directory-level
     fingerprint) so adding a new target to opam-repo doesn't nuke
     the cache for every existing target. Failed solves are simply
     absent on disk; they get re-attempted on the next run, which
     is usually what we want.

     {b Format compatibility:} the same files are read by the day11
     CLI via {!Day11_batch.Incremental_solver.load}. day11 CLI
     writes entries with [cache_key = None]; the load path here
     accepts those (treats them as a cache hit) so command-line and
     server-side runs share the cache without conflict. *)
  let solution_filename pkg = OpamPackage.to_string pkg ^ ".json"

  let compute_cache_key ~compiler_tag ~commit ~repos_digest ~pinned_versions =
    (* Append the pins only when non-empty so unpinned profiles keep
       the historical key form and don't force a mass re-solve. When
       pins change, the key changes and stale per-target solutions are
       invalidated automatically (previously they were silently
       reused, so a pin edit had no effect until manual deletion). *)
    let pins =
      match List.sort compare pinned_versions with
      | [] -> ""
      | l -> "|pins:" ^ String.concat "," l
    in
    Digest.to_hex
      (Digest.string (compiler_tag ^ "|" ^ commit ^ "|" ^ repos_digest ^ pins))

  (* ── Incremental reuse from the previous snapshot ────────────────

     A new opam-repo commit mints a new snapshot with an empty
     [solutions/] dir, and the commit is baked into [cache_key], so
     without help every target re-solves even though most commits
     touch a handful of packages. Before solving, seed the dir from
     the chronologically-previous snapshot: compute the set of package
     names whose [packages/<name>] tree changed between the two
     snapshots (per repo, both diff directions so added packages
     count) and carry over every cached solution whose [examined] set
     doesn't intersect it, re-stamped with this snapshot's [cache_key]
     ({!Day11_batch.Incremental_solver.reuse_solutions} with
     [~rekey_to]).

     Soundness: a solve's result can only change if some package name
     it examined changed, so a disjoint examined set means the cached
     result still holds. The [expected_cache_key] gate only trusts
     entries that were valid {e at the previous snapshot} (same
     compiler/pins, that snapshot's commit + repos digest) — leftovers
     from an interrupted run under different inputs are ignored. Reuse
     composes across snapshot chains by induction: each hop
     re-validates against that hop's diff before re-stamping.

     Any failure — no previous snapshot, the profile's repo set
     changed, a commit no longer resolvable locally (pruned by fetch)
     — logs and falls back to the plain full-solve path. *)

  (* Most recent other snapshot (by its repos.json [created] stamp)
     that has a [solutions/] dir on disk. Sorted by [created], not dir
     mtime: file regeneration inside a snapshot bumps mtimes and can
     float an old snapshot above the true predecessor. *)
  let find_previous_snapshot ~base ~current_key =
    match Bos.OS.Dir.contents base with
    | Error _ -> None
    | Ok entries -> (
        entries
        |> List.filter_map (fun p ->
               if String.equal (Fpath.basename p) current_key then None
               else
                 match Day11_batch.Snapshot.load p with
                 | Ok s ->
                     let sols = Fpath.(p / "solutions") in
                     if Bos.OS.Dir.exists sols |> Result.value ~default:false
                     then Some (s, sols)
                     else None
                 | Error _ -> None)
        |> List.sort
             (fun
               ((a : Day11_batch.Snapshot.t), _)
               ((b : Day11_batch.Snapshot.t), _)
             -> compare b.created a.created)
        |> function
        | [] -> None
        | x :: _ -> Some x)

  (* Package names changed between two snapshots, unioned across every
     repo and across both diff directions (the tree diff is
     asymmetric; the reverse direction catches added packages).
     [Error] when the repo sets differ or a commit can't be resolved
     locally — callers must treat that as "cannot bound the change". *)
  let changed_packages_lwt ~prev_repos ~cur_repos =
    let open Lwt.Syntax in
    let sorted_paths l = List.sort compare (List.map fst l) in
    if sorted_paths prev_repos <> sorted_paths cur_repos then
      Lwt.return (Error "repo set changed between snapshots")
    else
      Lwt.catch
        (fun () ->
          let rec go acc = function
            | [] -> Lwt.return (Ok acc)
            | (path, cur_sha) :: rest ->
                let prev_sha = List.assoc path prev_repos in
                if String.equal prev_sha cur_sha then go acc rest
                else
                  let* store, prev_h =
                    Day11_opam.Git_utils.get_git_repo_store_and_hash_commit_lwt
                      path (Some prev_sha)
                  in
                  let* cur_h =
                    Day11_opam.Git_utils.resolve_commit_in_store_lwt store
                      (Some cur_sha)
                  in
                  let* d1 =
                    Day11_opam.Git_packages.diff_packages_lwt ~store prev_h
                      cur_h
                  in
                  let* d2 =
                    Day11_opam.Git_packages.diff_packages_lwt ~store cur_h
                      prev_h
                  in
                  let acc =
                    List.fold_left
                      (fun s n -> OpamPackage.Name.Set.add n s)
                      acc (d1 @ d2)
                  in
                  go acc rest
          in
          go OpamPackage.Name.Set.empty cur_repos)
        (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

  (* Cooperative yield for the file sweeps below (17k+ entries each):
     they run on the daemon's single domain and would otherwise starve
     the event loop for seconds. No-op outside an Eio fiber. *)
  let cooperative_yield =
    let n = ref 0 in
    fun () ->
      incr n;
      if !n land 63 = 0 then try Eio.Fiber.yield () with _ -> ()

  (* Everything [execute_reuse] needs, computed on the Lwt side (the
     git diff uses the Lwt-native APIs); the sweeps themselves run in
     the op's Eio section so they can yield. *)
  type reuse_plan = {
    rp_changed : OpamPackage.Name.Set.t;
    rp_prev_snapshot : string; (* for logging *)
    rp_prev_cache_key : string;
    rp_prev_solutions : Fpath.t;
    rp_prev_tool_key : string;
    rp_new_tool_key : string;
    rp_prev_tool_dir : Fpath.t;
    rp_cur_tool_dir : Fpath.t;
    rp_missing : OpamPackage.t list;
    rp_tool_stems : string list;
  }

  let incremental_reuse_plan ~job ctx (key : Key.t) ~compiler_tag ~cache_key:_
      ~dir =
    let open Lwt.Syntax in
    (* Only targets with no file at all: a file that exists but fails
       the cache-key check means compiler/pins changed within this
       snapshot — the previous snapshot's entries would fail the
       [expected_cache_key] gate for the same reason, so there is
       nothing to gain from looking there. *)
    let missing =
      List.filter
        (fun pkg ->
          not
            (Sys.file_exists
               (Fpath.to_string Fpath.(dir / solution_filename pkg))))
        key.targets
    in
    let base = profile_snapshots_base ctx in
    let current_key = Day11_batch.Snapshot.compute_key ctx.repos_with_shas in
    match find_previous_snapshot ~base ~current_key with
    | None -> Lwt.return_none
    | Some ((prev : Day11_batch.Snapshot.t), prev_solutions) -> (
        (* Tool solves live beside the solutions with the same envelope
         (see {!Day11_batch.Incremental_solver.tool_solutions_dirname});
         carry the ones missing here over with the same diff. The stems
         encode the compiler pin, so they come from a directory listing
         rather than the target list. *)
        let tool_sub = Day11_batch.Incremental_solver.tool_solutions_dirname in
        let prev_tool_dir = Fpath.(parent prev_solutions / tool_sub) in
        let cur_tool_dir = Fpath.(parent dir / tool_sub) in
        let tool_stems =
          match Bos.OS.Dir.contents prev_tool_dir with
          | Error _ -> []
          | Ok entries ->
              List.filter_map
                (fun p ->
                  if Fpath.has_ext ".json" p then
                    let stem = Fpath.(basename (rem_ext p)) in
                    if
                      Sys.file_exists
                        (Fpath.to_string
                           Fpath.(cur_tool_dir / (stem ^ ".json")))
                    then None
                    else Some stem
                  else None)
                entries
        in
        if missing = [] && tool_stems = [] then Lwt.return_none
        else
          let* changed =
            changed_packages_lwt ~prev_repos:prev.repos
              ~cur_repos:ctx.repos_with_shas
          in
          match changed with
          | Error msg ->
              Current.Job.log job
                "incremental: cannot diff against previous snapshot %s (%s); \
                 solving from scratch"
                prev.key msg;
              Lwt.return_none
          | Ok changed -> (
              let mainline_path =
                match ctx.repos_with_shas with
                | (p, _) :: _ -> p
                | [] -> "" (* unreachable: profiles require >= 1 repo *)
              in
              match List.assoc_opt mainline_path prev.repos with
              | None -> Lwt.return_none
              | Some prev_commit ->
                  let prev_cache_key =
                    compute_cache_key ~compiler_tag ~commit:prev_commit
                      ~repos_digest:(repos_digest prev.repos)
                      ~pinned_versions:key.pinned_versions
                  in
                  Lwt.return_some
                    {
                      rp_changed = changed;
                      rp_prev_snapshot = prev.key;
                      rp_prev_cache_key = prev_cache_key;
                      rp_prev_solutions = prev_solutions;
                      rp_prev_tool_key =
                        Day11_batch.Incremental_solver.tool_cache_key
                          ~repos:prev.repos;
                      rp_new_tool_key =
                        Day11_batch.Incremental_solver.tool_cache_key
                          ~repos:ctx.repos_with_shas;
                      rp_prev_tool_dir = prev_tool_dir;
                      rp_cur_tool_dir = cur_tool_dir;
                      rp_missing = missing;
                      rp_tool_stems = tool_stems;
                    }))

  (* The sweeps: run in the op's Eio section (see [build]), yielding
     as they go. [dir] is the current snapshot's solutions dir. *)
  let execute_reuse ~job ~cache_key ~dir plan =
    let reused =
      if plan.rp_missing = [] then 0
      else
        Day11_batch.Incremental_solver.reuse_solutions
          ~expected_cache_key:plan.rp_prev_cache_key ~rekey_to:cache_key
          ~yield:cooperative_yield ~solutions_cache_dir:dir
          ~previous_dir:plan.rp_prev_solutions ~changed_packages:plan.rp_changed
          ~packages:(List.map OpamPackage.to_string plan.rp_missing)
          ()
    in
    let tools_reused =
      if plan.rp_tool_stems = [] then 0
      else (
        ignore (Bos.OS.Dir.create ~path:true plan.rp_cur_tool_dir);
        Day11_batch.Incremental_solver.reuse_solutions
          ~expected_cache_key:plan.rp_prev_tool_key
          ~rekey_to:plan.rp_new_tool_key ~yield:cooperative_yield
          ~solutions_cache_dir:plan.rp_cur_tool_dir
          ~previous_dir:plan.rp_prev_tool_dir ~changed_packages:plan.rp_changed
          ~packages:plan.rp_tool_stems ())
    in
    Current.Job.log job
      "incremental: %d package(s) changed since snapshot %s; reused %d/%d \
       cached solutions, %d/%d tool solves"
      (OpamPackage.Name.Set.cardinal plan.rp_changed)
      plan.rp_prev_snapshot reused
      (List.length plan.rp_missing)
      tools_reused
      (List.length plan.rp_tool_stems)

  (* Split [targets] three ways: cached solutions still valid for
     [cache_key]; cached {e failures} still valid; and targets that
     need (re)solving.

     A failure counts as cached only on a {b strict} key match (not
     the lenient {!is_cache_key_valid}, which accepts key-less legacy
     entries): a day11-CLI-written failure carries no key and no
     provenance, so re-attempting it stays the safe default. A
     strictly-keyed failure was either solved under exactly these
     inputs or carried forward by the incremental pass after checking
     its examined set against the commits' changed packages — in both
     cases re-solving is provably futile, and failed solves are the
     expensive (exhaustive-search) kind, ~85% of a steady-state
     commit's solver time before this shortcut. *)
  let partition_cached ~dir ~cache_key targets =
    if not (Bos.OS.Dir.exists dir |> Result.value ~default:false) then
      ([], [], targets)
    else
      List.fold_left
        (fun (cached, failed, uncached) pkg ->
          cooperative_yield ();
          let path = Fpath.(dir / solution_filename pkg) in
          match Day11_batch.Incremental_solver.load path with
          | Ok entry
            when Day11_batch.Incremental_solver.is_cache_key_valid
                   ~expected:(Some cache_key) entry -> (
              match entry with
              | Cached_solution { result; _ } ->
                  let result_json =
                    Yojson.Safe.to_string
                      (Day11_solution.Solve_result.to_json result)
                  in
                  ( (OpamPackage.to_string pkg, result_json) :: cached,
                    failed,
                    uncached )
              | Cached_failure { cache_key = Some k; _ }
                when String.equal k cache_key ->
                  (cached, pkg :: failed, uncached)
              | Cached_failure _ -> (cached, failed, pkg :: uncached))
          | _ -> (cached, failed, pkg :: uncached))
        ([], [], []) targets

  let save_result ~dir ~cache_key pkg result =
    let path = Fpath.(dir / solution_filename pkg) in
    let entry =
      Day11_batch.Incremental_solver.Cached_solution
        { package = pkg; result; cache_key = Some cache_key }
    in
    ignore (Day11_batch.Incremental_solver.save path entry)

  (* Persist a solve failure alongside the solutions (same
     [<pkg>.<ver>.json] path, [failed:true] + the solver's [error]).
     Previously discarded, which left the web with nothing to show for a
     package that never got past solving; now the per-version page can
     surface the solver's explanation. *)
  let save_failure ~dir ~cache_key pkg ~error ~examined =
    let path = Fpath.(dir / solution_filename pkg) in
    let entry =
      Day11_batch.Incremental_solver.Cached_failure
        { package = pkg; error; examined; cache_key = Some cache_key }
    in
    ignore (Day11_batch.Incremental_solver.save path entry)

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    let compiler_tag =
      if key.ocaml_version = "" then "none" else key.ocaml_version
    in
    let cache_key =
      compute_cache_key ~compiler_tag ~commit:key.commit
        ~repos_digest:key.repos_digest ~pinned_versions:key.pinned_versions
    in
    let dir = snapshot_solutions_dir ctx in
    ignore (Bos.OS.Dir.create ~path:true dir);
    (* Seed from the previous snapshot before partitioning, so a repo
       bump only re-solves targets whose examined set intersects the
       commits' changed packages. The git diff runs here on the Lwt
       side (Lwt-native APIs); the file sweeps run below inside
       [run_eio] where they can cooperatively yield. *)
    let* reuse_plan =
      incremental_reuse_plan ~job ctx key ~compiler_tag ~cache_key ~dir
    in
    (* Consolidated list of targets that failed to solve, written next to
       the snapshot's other summaries as [solve_failures.json] (a JSON
       array of "name.version"). The snapshot page reads this one file
       for its "Solve failures" section rather than scanning the ~17k
       per-target solution files. Includes both freshly-failed targets
       and cached failures (strict-key hits that were not re-solved), so
       whenever the solver runs this is the complete current set. *)
    let write_solve_failures failed =
      let path = Fpath.(parent dir / "solve_failures.json") in
      let json =
        `List (List.map (fun s -> `String s) (List.sort compare failed))
      in
      ignore (Bos.OS.File.write path (Yojson.Safe.to_string json))
    in
    Lwt_eio.run_eio @@ fun () ->
    (match reuse_plan with
    | None -> ()
    | Some plan -> execute_reuse ~job ~cache_key ~dir plan);
    let cached, cached_failures, uncached =
      partition_cached ~dir ~cache_key key.targets
    in
    let cached_failure_strs = List.map OpamPackage.to_string cached_failures in
    let n_cached = List.length cached in
    let n_failed_cached = List.length cached_failures in
    let n_uncached = List.length uncached in
    let short_commit =
      String.sub key.commit 0 (min 12 (String.length key.commit))
    in
    if n_uncached = 0 then (
      Current.Job.log job
        "[profile %s] All cached: %d solutions, %d failures (commit %s) — \
         skipping solver"
        ctx.profile_name n_cached n_failed_cached short_commit;
      write_solve_failures cached_failure_strs;
      Ok Value.{ results = cached })
    else (
      Current.Job.log job
        "[profile %s] %d cached (+%d cached failures), solving %d new/stale \
         targets (commit %s)"
        ctx.profile_name n_cached n_failed_cached n_uncached short_commit;
      Eio.Switch.run @@ fun sw ->
      let results =
        Day11_solver_pool.Solver_pool.solve_many ~sw ctx.env
          ?ocaml_version:ctx.ocaml_version ~constraints:ctx.pinned_versions
          ~on_progress:(fun ~done_count ~total ->
            Current.Job.log job "Solving: %d/%d" done_count total)
          ~np:ctx.np ~repos:ctx.repos_with_shas uncached
      in
      let new_pairs =
        List.filter_map
          (fun (pkg, result) ->
            match result with
            | Ok solve_result ->
                save_result ~dir ~cache_key pkg solve_result;
                let result_json =
                  Day11_solution.Solve_result.to_json solve_result
                in
                Some
                  (OpamPackage.to_string pkg, Yojson.Safe.to_string result_json)
            | Error (error, examined) ->
                save_failure ~dir ~cache_key pkg ~error ~examined;
                None)
          results
      in
      write_solve_failures
        (cached_failure_strs
        @ List.filter_map
            (fun (pkg, r) ->
              match r with
              | Error _ -> Some (OpamPackage.to_string pkg)
              | Ok _ -> None)
            results);
      Current.Job.log job
        "Solved %d/%d new targets; %d cached → %d total solutions"
        (List.length new_pairs) n_uncached n_cached
        (List.length new_pairs + n_cached);
      Ok Value.{ results = cached @ new_pairs })
end

module Solver_cache = Current_cache.Make (SolveOp)

type solution = {
  target : OpamPackage.t;
  solve_result : Day11_solution.Solve_result.t;
}

(* Read the consolidated [solve_failures.json] the solver writes next to a
   snapshot (see [write_solve_failures]): a JSON array of "name.version".
   Returns [] if the file is absent or unparseable. *)
let read_solve_failures ~snapshot_dir =
  match Bos.OS.File.read Fpath.(snapshot_dir / "solve_failures.json") with
  | Error _ -> []
  | Ok data -> (
      match try Some (Yojson.Safe.from_string data) with _ -> None with
      | Some (`List items) ->
          List.filter_map (function `String s -> Some s | _ -> None) items
      | _ -> [])

(* [tracks] carries one [(commit, packages)] tracking result per entry
   of [repos_with_shas], in the same order. {!Track.v} is latched:
   right after a repo moves, its track current still reports the
   previous commit's packages while the re-track runs. If we solved on
   that torn state (new snapshot dir + old target list — or the
   reverse) we'd waste a full solve per commit and could write
   mixed-provenance solutions into the snapshot; instead, any
   evaluation whose track commits don't all match [repos_with_shas] is
   answered with a plain "still running" output — the consistent
   evaluation always follows once the re-track lands. *)

(** Solve all tracked packages using day11's solver. Returns solutions keyed by
    target package. *)
let solve ~env ~np ~profile_name ~repos_with_shas ?ocaml_version
    ?(pinned_versions = []) ~cache_dir
    (tracks : (string * Track.t list) Current.t list) =
  let open Current.Syntax in
  Current.component "[%s] day11-solve" profile_name
  |>
  let> tracks = Current.list_seq tracks in
  let consistent =
    List.compare_lengths tracks repos_with_shas = 0
    && List.for_all2
         (fun (_, sha) (track_commit, _) -> String.equal sha track_commit)
         repos_with_shas tracks
  in
  if not consistent then
    (* Not an error: a latched input hasn't caught up with the repo
       state yet. Report "active" so downstream stays pending until
       the consistent evaluation replaces this one. *)
    Current_incr.const (Error (`Active `Ready), None)
  else
    let tracked = Track.merge_values (List.map snd tracks) in
    let commit_hash =
      match repos_with_shas with
      | (_, sha) :: _ -> sha
      | [] -> "" (* unreachable: profiles require >= 1 repo *)
    in
    let targets = List.map Track.pkg tracked in
    let ocaml_version_str =
      match ocaml_version with
      | Some pkg -> OpamPackage.to_string pkg
      | None -> ""
    in
    let pinned_strs = List.map OpamPackage.to_string pinned_versions in
    Solver_cache.get
      {
        repos_with_shas;
        env;
        np;
        profile_name;
        ocaml_version;
        pinned_versions;
        cache_dir;
      }
      SolveOp.Key.
        {
          targets;
          commit = commit_hash;
          repos_digest = repos_digest repos_with_shas;
          ocaml_version = ocaml_version_str;
          pinned_versions = pinned_strs;
        }
    |> Current.Primitive.map_result
         (Result.map (fun v ->
              List.filter_map
                (fun (pkg_str, json_str) ->
                  try
                    let pkg = OpamPackage.of_string pkg_str in
                    let json = Yojson.Safe.from_string json_str in
                    match Day11_solution.Solve_result.of_json json with
                    | Ok result -> Some { target = pkg; solve_result = result }
                    | Error _ -> None
                  with _ -> None)
                v.SolveOp.Value.results))
