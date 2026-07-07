(** Day11-based solver: in-process solving via solver_pool subprocesses.

    Replaces the Cap'n Proto solver service. Uses day11's solver_worker
    binaries for parallel solving, communicating via JSONL files. *)

module SolveOp = struct
  type t = {
    repos_with_shas : (string * string) list;
    env : Eio_unix.Stdenv.base;
    np : int;
    profile_name : string;
    ocaml_version : OpamPackage.t option;
    pinned_versions : OpamPackage.t list;
    (** Hard version pins fed into the solver as [(`Eq, v)]
        constraints. Empty list = no extra pins beyond
        [ocaml_version]. Surfaced via [Profile.pinned_versions];
        used to propagate a specific +ox / variant flavour
        through transitive deps. *)
    cache_dir : Fpath.t;
    (** Used to derive the per-snapshot [solutions/] directory
        ([snapshot_dir/solutions/<pkg>.json]). The on-disk cache lets
        a pipeline restart after [sqlite.db] has been wiped (or after
        OCurrent's primitive cache otherwise loses state) skip the
        ~3-minute solver pass entirely as long as repos haven't moved
        and the compiler / target set is unchanged. *)
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
      t.commit ^ "@" ^ t.repos_digest ^ "|" ^ t.ocaml_version ^
      "|" ^ String.concat "," t.pinned_versions ^ ":" ^
      (List.map OpamPackage.to_string t.targets
       |> String.concat ",")
  end

  module Value = struct
    type t = {
      results : (string * string) list;
      (* (pkg_str, solve_result_json) pairs *)
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-solver"

  let pp f (key : Key.t) =
    Fmt.pf f "solve %d packages @%s"
      (List.length key.targets)
      (String.sub key.commit 0 (min 12 (String.length key.commit)))

  let auto_cancel = false

  let snapshot_solutions_dir ctx =
    let snapshot_dir =
      Day11_profile_ctx_loader.snapshot_dir_of
        ~cache_dir:ctx.cache_dir
        ~profile_name:ctx.profile_name
        ctx.repos_with_shas
    in
    Fpath.(snapshot_dir / "solutions")

  (* Per-target solution cache.

     Files live at [snapshot_dir/solutions/<pkg>.<ver>.json] and are
     read/written via {!Day11_batch.Incremental_solver}. We embed a
     [cache_key = hash(compiler ∥ commit ∥ repos_digest)] in each
     entry so a repo or compiler change invalidates only the
     affected entries — other cached solutions stay valid.

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
  let solution_filename pkg =
    OpamPackage.to_string pkg ^ ".json"

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
    Digest.to_hex (Digest.string
      (compiler_tag ^ "|" ^ commit ^ "|" ^ repos_digest ^ pins))

  (* Split [targets] into those whose cached solutions are still
     valid (matching [cache_key]) and those that need (re)solving. *)
  let partition_cached ~dir ~cache_key targets =
    if not (Bos.OS.Dir.exists dir |> Result.value ~default:false)
    then ([], targets)
    else
      List.fold_left (fun (cached, uncached) pkg ->
        let path = Fpath.(dir / solution_filename pkg) in
        match Day11_batch.Incremental_solver.load path with
        | Ok entry
          when Day11_batch.Incremental_solver.is_cache_key_valid
                 ~expected:(Some cache_key) entry ->
          (match entry with
           | Cached_solution { result; _ } ->
             let result_json =
               Yojson.Safe.to_string
                 (Day11_solution.Solve_result.to_json result) in
             (OpamPackage.to_string pkg, result_json) :: cached, uncached
           | Cached_failure _ -> cached, pkg :: uncached)
        | _ -> cached, pkg :: uncached
      ) ([], []) targets

  let save_result ~dir ~cache_key pkg result =
    let path = Fpath.(dir / solution_filename pkg) in
    let entry = Day11_batch.Incremental_solver.Cached_solution {
      package = pkg;
      result;
      cache_key = Some cache_key;
    } in
    ignore (Day11_batch.Incremental_solver.save path entry)

  (* Persist a solve failure alongside the solutions (same
     [<pkg>.<ver>.json] path, [failed:true] + the solver's [error]).
     Previously discarded, which left the web with nothing to show for a
     package that never got past solving; now the per-version page can
     surface the solver's explanation. *)
  let save_failure ~dir ~cache_key pkg ~error ~examined =
    let path = Fpath.(dir / solution_filename pkg) in
    let entry = Day11_batch.Incremental_solver.Cached_failure {
      package = pkg;
      error;
      examined;
      cache_key = Some cache_key;
    } in
    ignore (Day11_batch.Incremental_solver.save path entry)

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Mostly_harmless in
    let compiler_tag =
      if key.ocaml_version = "" then "none" else key.ocaml_version in
    let cache_key =
      compute_cache_key ~compiler_tag
        ~commit:key.commit ~repos_digest:key.repos_digest
        ~pinned_versions:key.pinned_versions in
    let dir = snapshot_solutions_dir ctx in
    ignore (Bos.OS.Dir.create ~path:true dir);
    (* Consolidated list of targets that failed to solve, written next to
       the snapshot's other summaries as [solve_failures.json] (a JSON
       array of "name.version"). The snapshot page reads this one file
       for its "Solve failures" section rather than scanning the ~17k
       per-target solution files. Failed targets are always re-solved
       (partition_cached treats a cached failure as uncached), so
       whenever the solver runs this is the complete current set. *)
    let write_solve_failures failed =
      let path = Fpath.(parent dir / "solve_failures.json") in
      let json =
        `List (List.map (fun s -> `String s) (List.sort compare failed)) in
      ignore (Bos.OS.File.write path (Yojson.Safe.to_string json))
    in
    Lwt_eio.run_eio @@ fun () ->
    let cached, uncached =
      partition_cached ~dir ~cache_key key.targets in
    let n_cached = List.length cached in
    let n_uncached = List.length uncached in
    let short_commit =
      String.sub key.commit 0 (min 12 (String.length key.commit)) in
    if n_uncached = 0 then begin
      Current.Job.log job
        "[profile %s] All %d solutions cached (commit %s) — skipping solver"
        ctx.profile_name n_cached short_commit;
      write_solve_failures [];
      Ok Value.{ results = cached }
    end else begin
      Current.Job.log job
        "[profile %s] %d cached, solving %d new/stale targets (commit %s)"
        ctx.profile_name n_cached n_uncached short_commit;
      Eio.Switch.run @@ fun sw ->
      let results =
        Day11_solver_pool.Solver_pool.solve_many ~sw ctx.env
          ?ocaml_version:ctx.ocaml_version
          ~constraints:ctx.pinned_versions
          ~on_progress:(fun ~done_count ~total ->
            Current.Job.log job "Solving: %d/%d" done_count total)
          ~np:ctx.np ~repos:ctx.repos_with_shas uncached
      in
      let new_pairs = List.filter_map (fun (pkg, result) ->
        match result with
        | Ok solve_result ->
          save_result ~dir ~cache_key pkg solve_result;
          let result_json = Day11_solution.Solve_result.to_json solve_result in
          Some (OpamPackage.to_string pkg, Yojson.Safe.to_string result_json)
        | Error (error, examined) ->
          save_failure ~dir ~cache_key pkg ~error ~examined;
          None
      ) results in
      write_solve_failures
        (List.filter_map (fun (pkg, r) ->
           match r with
           | Error _ -> Some (OpamPackage.to_string pkg)
           | Ok _ -> None) results);
      Current.Job.log job
        "Solved %d/%d new targets; %d cached → %d total solutions"
        (List.length new_pairs) n_uncached n_cached
        (List.length new_pairs + n_cached);
      Ok Value.{ results = cached @ new_pairs }
    end
end

module Solver_cache = Current_cache.Make (SolveOp)

type solution = {
  target : OpamPackage.t;
  solve_result : Day11_solution.Solve_result.t;
}

(** Solve all tracked packages using day11's solver. Returns solutions
    keyed by target package. *)
let repos_digest repos =
  let sorted = List.sort compare
    (List.map (fun (path, sha) -> path ^ "@" ^ sha) repos) in
  Digest.to_hex (Digest.string (String.concat "\n" sorted))

let solve ~env ~np ~profile_name ~repos_with_shas ?ocaml_version
    ?(pinned_versions = [])
    ~cache_dir
    ~(opam_commit : Current_git.Commit.t Current.t)
    (tracked : Track.t list Current.t) =
  let open Current.Syntax in
  Current.component "[%s] day11-solve" profile_name
  |>
  let> tracked and> commit = opam_commit in
  let commit_hash = Current_git.Commit.id commit
    |> Current_git.Commit_id.hash in
  let targets = List.map Track.pkg tracked in
  let ocaml_version_str = match ocaml_version with
    | Some pkg -> OpamPackage.to_string pkg
    | None -> "" in
  let pinned_strs = List.map OpamPackage.to_string pinned_versions in
  Solver_cache.get
    { repos_with_shas; env; np; profile_name; ocaml_version;
      pinned_versions; cache_dir }
    SolveOp.Key.{
      targets;
      commit = commit_hash;
      repos_digest = repos_digest repos_with_shas;
      ocaml_version = ocaml_version_str;
      pinned_versions = pinned_strs;
    }
  |> Current.Primitive.map_result (Result.map (fun v ->
    List.filter_map (fun (pkg_str, json_str) ->
      try
        let pkg = OpamPackage.of_string pkg_str in
        let json = Yojson.Safe.from_string json_str in
        match Day11_solution.Solve_result.of_json json with
        | Ok result -> Some { target = pkg; solve_result = result }
        | Error _ -> None
      with _ -> None
    ) v.SolveOp.Value.results))
