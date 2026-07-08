open Docs_ci_lib
module Profile = Day11_batch.Profile
module Profile_ctx = Day11_batch.Profile_ctx

(* Public alias so [Ocaml_docs_ci] can pass profile contexts in
   without depending on the internal type. *)
type profile_ctx = Profile_ctx.t

(* Poll the base image digest once a day. The result feeds
   [Day11_base.ensure], so a new upstream image triggers a fresh
   [docker build], and the digest flows into the base layer hash —
   every dependent build layer rebuilds when the base changes. *)
let base_digest_schedule =
  Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

(* Per-profile doc sub-pipeline. Each profile carries its own:
   - opam_repositories (repos_with_shas + git_packages indexed from them)
   - os_distribution / os_version / arch (→ base image + os_dir)
   - base_image_digest (optional pinning)
   - html_dir (per-profile output; [None] = build-only)
   All profiles share the global [config] and the tracked [opam]
   commit (the source of truth for which packages to track).

   The base image (+ opam-build binary) is built via the
   {!Day11_base.ensure} OCurrent op, which surfaces in [/jobs] with
   live docker build logs. The rest of the pipeline is gated on
   that job succeeding. *)
(* Single process-wide build pool. Must be created once, not inside
   the pipeline body — the Engine re-evaluates the body on every
   tick, and a fresh pool per tick means old in-flight jobs keep
   their slots while new ones draw from a new budget, overshooting
   [Config.jobs]. One pool across profiles also prevents the total
   concurrency from scaling with the number of profiles. *)
let build_pool : unit Current.Pool.t option ref = ref None

let get_build_pool capacity =
  match !build_pool with
  | Some p -> p
  | None ->
    let p = Current.Pool.create ~label:"day11-builds" capacity in
    build_pool := Some p;
    p

let v_for_profile ~config ~eio_env ~cache_dir:_ ?cpu_slots
    ~(tracking_commits : Current_git.Commit.t Current.t list)
    (ctx_current : profile_ctx Current.t) =
  let open Current.Syntax in
  let* (ctx : profile_ctx) = ctx_current in
  let ctx = match cpu_slots with
    | Some pool -> Profile_ctx.with_cpu_slots ctx pool
    | None -> ctx in
  let profile = ctx.profile in
  let env = eio_env in
  ignore (Bos.OS.Dir.create ~path:true ctx.os_dir);
  (* Resolve the current arch-specific digest of the upstream image
     on a daily schedule. Changes flow into [Day11_base.ensure] and
     — via [Profile_ctx.with_base_digest] — into the base layer hash,
     so every dependent build rehashes when the upstream image moves. *)
  let digest =
    Day11_base_digest.current
      ~schedule:base_digest_schedule
      ~image:(Profile.base_image_tag profile)
      ~arch:profile.arch
  in
  (* Wait for base image + opam-build binary; shown as an OCurrent
     job with [docker build] output visible to the web UI. Also
     threads the digest into the base layer hash so every build
     layer rebuilds when the upstream image changes. *)
  let* d = digest in
  let ctx = Profile_ctx.with_base_digest ctx d in
  (* Self-heal a stale ensure-base success whose base dir was removed
     out-of-band (an os_dir migration, a cache wipe). The op is keyed on
     profile + digest and doesn't re-check the base dir, so a cached
     success would otherwise make [ensure] a no-op and wedge every
     dependent build at [require_base]. If the base isn't materialised,
     invalidate so [ensure] actually rebuilds it. Mirrors
     [Day11_prep.reconcile_cache] for layers. *)
  (match Profile_ctx.require_base ctx with
   | Ok _ -> ()
   | Error _ -> Day11_base.invalidate ~image_digest:d ctx);
  (* Wait for base image + opam-build binary; shown as an OCurrent
     job with [docker build] output visible to the web UI. *)
  let base_ready = Day11_base.ensure ~env ~digest ctx in
  let* () = base_ready in
  (* After [Day11_base.ensure] succeeds the base layer is on disk;
     require_base is a cheap marker-file check. *)
  let ctx = match Profile_ctx.require_base ctx with
    | Ok c -> c
    | Error (`Msg msg) ->
      Logs.err (fun f -> f "[%s] %s" profile.name msg);
      failwith msg
  in
  let benv = ctx.benv in

  (* 1) Track packages — per profile, honouring [target_mode]. The
     global config's [--limit] / [--filter] act as a fallback when
     the profile doesn't constrain things (All_versions + no named
     packages). *)
  let profile_limit = Profile.track_limit profile in
  let profile_filter = Profile.track_filter profile in
  let limit = match profile_limit with
    | Some _ as l -> l
    | None -> Config.take_n_last_versions config
  in
  let filter = match profile_filter with
    | [] -> Config.track_packages config
    | f -> f
  in
  (* Fan out [Track.v] across all of [profile.opam_repositories] and
     merge. [repo_label] (the static path string, stable across
     ticks) distinguishes per-repo components so OCurrent doesn't
     collide different repos into one "instance" when multiple
     profiles share the same filter+limit. *)
  let tracked =
    List.map2
      (fun repo_label commit ->
        Track.v ~repo_label ~limit ~filter commit)
      profile.opam_repositories tracking_commits
    |> Track.merge
  in

  (* 2) Solve against this profile's repo set. Pin the compiler to
     the profile's [compiler] field so solutions honour the profile
     (e.g. [ocaml-variants.5.2.0+ox] for the oxcaml profile). Without
     this the solver falls back to mainline ocaml-base-compiler and
     picks mainline dune/ppxlib/etc, ignoring oxcaml overlays. *)
  let pinned_versions =
    List.filter_map (fun s ->
      try Some (OpamPackage.of_string s)
      with _ ->
        Logs.warn (fun m -> m "[%s] ignoring malformed pinned_versions entry %S"
          profile.name s);
        None
    ) profile.pinned_versions
  in
  let solutions =
    Day11_solver.solve ~env ~np:(Config.jobs config)
      ~profile_name:profile.name
      ~repos_with_shas:ctx.repos_with_shas
      ?ocaml_version:ctx.ocaml_version
      ~pinned_versions
      ~cache_dir:ctx.cache_dir
      (* [opam_commit] drives the solver's re-run trigger and goes
         into the cache key. Pick mainline (first entry) — overlay
         commits already flow through [repos_with_shas] which is
         part of the same cache key, so this doesn't lose anything. *)
      ~opam_commit:(List.hd tracking_commits) tracked
  in
  let* solutions in
  (* Drop solutions whose [build_deps] graph contains a cycle (the
     DAG-construction recursion in [Dag.build_dag] only memoises
     {e after} recursing, so a build-deps cycle stack-overflows it).
     We do NOT filter on cyclic [doc_deps]: opam routinely produces
     post-style cycles via [{post & with-doc}] and [x-extra-doc-deps]
     guards, and filtering them would drop nearly every package.
     [Day11_solution.Deps.transitive_deps] is cycle-safe (uses a
     [visiting] guard) so all the doc-deps walks tolerate them. *)
  let solutions, dropped =
    List.partition (fun (s : Day11_solver.solution) ->
      not (Day11_solution.Deps.has_cycle s.solve_result.build_deps))
      solutions in
  List.iter (fun (s : Day11_solver.solution) ->
    Log.warn (fun f -> f "[%s] dropping %s: solver output has dep cycle"
      profile.name (OpamPackage.to_string s.target))) dropped;
  Log.info (fun f -> f "[%s] solved %d packages (%d dropped for cycles)"
    profile.name (List.length solutions) (List.length dropped));

  (* 3) Build DAG — reuse the shared hash cache from the ctx. *)
  let build_solutions = List.map (fun (s : Day11_solver.solution) ->
    (s.target, s.solve_result.build_deps, s.solve_result.doc_deps)
  ) solutions in
  let nodes = Day11_opam_build.Dag.build_dag ctx.hash_cache
    ~base_hash:ctx.base.hash build_solutions in
  Log.info (fun f -> f "[%s] %d build nodes"
    profile.name (List.length nodes));

  (* 4) Doc plan (only if this profile has an html_dir).

     Both doc and build containers mount a single merged opam-repo
     assembled from all of [profile.opam_repositories] (earlier wins
     at top-level; later repos override at the packages/ level). We
     build it once per snapshot and reuse the same mount for both
     sets of containers — matching [day11 build]'s setup, where the
     merged repo shadows [container_backend]'s per-node temp-repo and
     opam sees the full package universe instead of just [node.pkg].
     That matters because opam's [installed_opams] lookup for packages
     marked installed by [Opamh.dump_state] falls through to the
     configured repo when the [.opam-switch/packages/<pkg>/opam] file
     isn't visible; without the full repo, opam reports those packages
     as "No definition found" and filter-expanding
     [%\{ocaml-config:share\}%] (inside [ocaml.X.Y]'s build) yields an
     empty string. *)
  let snapshot_dir =
    Day11_profile_ctx_loader.snapshot_dir_of
      ~cache_dir:ctx.cache_dir
      ~profile_name:profile.name
      ctx.repos_with_shas in
  (* NB: we deliberately do NOT mount a full merged opam-repository at
     [/home/opam/.opam/repo/default]. Each build mounts a per-package
     slice there (Container_backend.build, from [~opam_repositories]),
     and a build only ever needs its own package (deps are pre-installed
     in the overlay). Mounting the full ~18k-package repo on top
     shadowed that slice and made opam canonicalise/scan all ~18k
     packages on every switch-state load — several seconds per node. *)
  let opam_build_mnt =
    match Day11_opam_build.Base.opam_build_mount
            ~cache_dir:ctx.cache_dir
            ?opam_build_repo:(Option.map Fpath.v profile.opam_build_repo)
            () with
    | Some m -> [ m ] | None -> []
  in
  let doc_mounts = opam_build_mnt in
  let build_mounts = opam_build_mnt in
  let opam_repos_fpath = List.map Fpath.v profile.opam_repositories in
  let target_solutions = List.map (fun (s : Day11_solver.solution) ->
    (s.target, s.solve_result)
  ) solutions in
  (* Real blessing: run the same [compute_blessings] the day11 CLI
     uses, so the per-build [blessed] flag written to history reflects
     the canonical universe choice (maximise deps_count, then
     revdeps_count, then compiler version). Without this every build
     lands as [blessed:false], and the GUI shows "0 blessed builds"
     even for latest-only profiles where one universe per package is
     expected to be blessed. *)
  let blessing_input = List.map
    (fun (t, (r : Day11_solution.Solve_result.t)) -> (t, r.build_deps))
    target_solutions in
  let blessing_maps = Day11_batch.Blessing.compute_blessings blessing_input in
  (* Set up [run_log] and [recorder] before [plan_doc_dag], so the
     dispatch wrapped inside [doc_plan.build_one] can record outcomes
     directly per-node. Recording at dispatch time means cascade-
     skipped nodes (which OCurrent never invokes) leave no stale
     "ran-and-failed" entry behind — their state is derivable from
     the persisted DAG. *)
  let packages_dir = Day11_batch.Snapshot.packages_dir snapshot_dir in
  ignore (Bos.OS.Dir.create ~path:true packages_dir);
  Day11_lib.Run_log.set_log_base_dir (Fpath.to_string snapshot_dir);
  let run_log = Day11_lib.Run_log.start_run () in
  let recorder = Day11_batch.Recorder.create
    ~env ~os_dir:ctx.os_dir ~packages_dir
    ~blessing_maps ~run_log in
  let on_pkg_complete node ~success =
    Metrics.record_build ~profile:profile.name ~success;
    Day11_batch.Recorder.record_build recorder node ~success in
  let on_doc_complete node ~blessed ~universe ~success =
    Metrics.record_doc ~profile:profile.name ~success ~blessed;
    Day11_batch.Recorder.record_doc recorder node ~blessed ~universe ~success in
  (* [plan_doc_dag] forks 9+ fibers (driver + per-compiler odoc),
     each running a [day11-solver-worker] subprocess. Subprocess
     awaits go through [Sys.Run.run] which yields on socket I/O,
     so under [Lwt_eio.with_event_loop] the cohttp web server gets
     scheduler cycles between yields and stays responsive. *)
  let doc_plan = match profile.html_dir with
    | None -> None
    | Some _ ->
      Eio.Switch.run @@ fun plan_sw ->
      Day11_doc.Generate.plan_doc_dag ~sw:plan_sw env ctx
        ~mounts:doc_mounts
        ~build_one:(fun node ->
          Eio.Switch.run @@ fun sw ->
          match Day11_opam_build.Build_layer.build ~sw env benv
                  ~opam_repositories:opam_repos_fpath
                  ~mounts:build_mounts node () with
          | Day11_opam_build.Types.Success _ -> true
          | _ -> false)
        ~on_pkg_complete ~on_doc_complete
        ~snapshot_dir
        ~nodes
        ~solutions:target_solutions ~blessing_maps ()
  in
  let all_dag_nodes = match doc_plan with
    | Some plan ->
      let nb, nt, nc, nd, nl = List.fold_left (fun (b, t, c, d, l) n ->
        match plan.node_kind n with
        | Day11_doc.Generate.Build -> (b+1, t, c, d, l)
        | Tool -> (b, t+1, c, d, l)
        | Compile -> (b, t, c+1, d, l)
        | Doc_all -> (b, t, c, d+1, l)
        | Link -> (b, t, c, d, l+1)
      ) (0, 0, 0, 0, 0) plan.all_nodes in
      Log.info (fun f -> f "[%s] doc DAG: %d total nodes \
                             (build=%d tool=%d compile=%d doc=%d link=%d)"
        profile.name (List.length plan.all_nodes) nb nt nc nd nl);
      plan.all_nodes
    | None -> nodes
  in
  let dispatch : Eio_unix.Stdenv.base -> Day11_opam_layer.Build.t -> bool =
    match doc_plan with
    | Some plan ->
      fun _env node ->
        Eio.Switch.run @@ fun sw -> plan.build_one ~sw _env node
    | None ->
      fun _env node ->
        Eio.Switch.run @@ fun sw ->
        match Day11_opam_build.Build_layer.build ~sw _env benv
                ~opam_repositories:opam_repos_fpath
                ~mounts:build_mounts node () with
        | Day11_opam_build.Types.Success _ -> true
        | _ -> false
  in
  let node_kind = match doc_plan with
    | Some plan -> plan.node_kind
    | None -> fun _ -> Day11_doc.Generate.Build
  in
  let pool = get_build_pool (Config.jobs config) in
  (* Persist the planned-node counters so mid-run CLIs
     ([day11 status / results / failures]) can report a denominator
     via [Day11_batch.Live_view]. *)
  let count_kind kind = match doc_plan with
    | None -> if kind = Day11_doc.Generate.Build
              then List.length nodes else 0
    | Some plan -> List.fold_left (fun acc n ->
        if plan.node_kind n = kind then acc + 1 else acc)
        0 plan.all_nodes in
  Day11_lib.Run_log.write_doc_dag run_log
    ~n_build:(count_kind Day11_doc.Generate.Build)
    ~n_tool:(count_kind Day11_doc.Generate.Tool)
    ~n_compile:(count_kind Day11_doc.Generate.Compile)
    ~n_doc_all:(count_kind Day11_doc.Generate.Doc_all)
    ~n_link:(count_kind Day11_doc.Generate.Link);
  let node_cache : (string, Day11_prep.t Current.t) Hashtbl.t =
    Hashtbl.create (List.length all_dag_nodes) in
  let rec make_node (dag_node : Day11_opam_layer.Build.t) =
    match Hashtbl.find_opt node_cache dag_node.hash with
    | Some existing -> existing
    | None ->
      let dep_currents = List.map make_node dag_node.deps in
      (* Gate this node on its deps for ordering + error cascade only;
         the dep *values* are no longer read (dispatch derives the
         overlay-stack dirs from the static DAG node), so collapse the
         edge to unit. [Current.all] is the tree-folded combinator, so
         the aggregation is O(log n) and eq-cuttable rather than a
         right-fold chain that re-allocates on every propagation. *)
      let deps = Current.all (List.map Current.ignore_value dep_currents) in
      let kind = node_kind dag_node in
      let label = match kind with
        | Day11_doc.Generate.Build -> "build"
        | Tool -> "tool"
        | Compile -> "compile"
        | Doc_all -> "doc"
        | Link -> "link"
      in
      (* Plain wiring — no [Current.catch], no synthesised fallback.
         Recording is handled at dispatch time (inside [doc_plan]'s
         [build_one] via [on_pkg_complete]/[on_doc_complete]), so when
         a dep is Error the entire subtree cascades to Error in the
         OCurrent graph and dispatch is never invoked. Cascade
         attribution is recoverable from [<snapshot_dir>/dag.json] +
         the per-package history. *)
      let node =
        Day11_prep.run_node ~env ~os_dir:ctx.os_dir ~pool ~dispatch
          ~label ~profile_name:profile.name ~dag_node ~deps ()
      in
      Hashtbl.replace node_cache dag_node.hash node;
      node
  in
  let all_nodes = List.map make_node all_dag_nodes in
  (* Collapse the build+doc subtree into a single node in the
     pipeline diagram. All the individual build/doc [Current.t]s
     are still created (so the [/jobs] page and per-node caching
     work as before), but [pipeline.svg] renders them as one "[+]"
     node per profile which the user can expand on demand. Without
     this, the diagram has thousands of nodes and [dot] can't
     render it.
     We [Current.catch] each node before [list_seq] purely so an
     individual node failing doesn't put the whole list_seq into
     Error and starve downstream cleanup of evaluations. The
     cascade still works at the per-node level because each
     [make_node] call wires its parent's deps to the un-caught
     [node] — caught only in this final aggregation. *)
  let collapsed_builds =
    Current.collapse
      ~key:"profile-builds" ~value:profile.name
      ~input:ctx_current
      (Current.list_seq (List.map Current.catch all_nodes))
  in
  let builds =
    let+ results = collapsed_builds in
    (* Only act once the run has completed — i.e. every planned node has
       a result and no build/doc job is still pending. We require one
       result per [all_dag_nodes] entry: anything short means the plan
       isn't fully resolved yet, so we write nothing (and, in particular,
       don't emit a partial [final_status.json] that the diff views would
       take as authoritative). This also guards the [List.map2] below. *)
    if List.compare_lengths results all_dag_nodes <> 0 then ()
    else
    (* Derive [status.json] from the collapsed build results joined with
       the plan — NOT from history.jsonl. [results] is one entry per plan
       node, cache hits included (an already-built layer resolves to
       [Ok]), so the counts reflect the full plan state rather than only
       what this run re-dispatched. [node_blessed]/[node_kind] come from
       the in-memory plan; no disk reads, no run-id matching. *)
    let node_blessed = match doc_plan with
      | Some p -> p.Day11_doc.Generate.node_blessed
      | None -> fun _ -> false in
    (* hash -> did this node build (or hit cache)? Used to tell a real
       failure from a cascade: a failed node is a cascade iff a direct
       dep also failed — the error propagated rather than originating
       here. A dep absent from the map (shouldn't happen for a closed
       plan) is treated as ok, so it can't spuriously mark a cascade. *)
    let ok_of = Hashtbl.create (List.length all_dag_nodes) in
    List.iter2 (fun (n : Day11_opam_layer.Build.t) res ->
      Hashtbl.replace ok_of n.hash (Result.is_ok res)) all_dag_nodes results;
    let dep_ok h = match Hashtbl.find_opt ok_of h with Some b -> b | None -> true in
    (* Per-node outcome, keyed by package, so we can feed both the
       aggregate totals ([status.json]) and the per-blessed-package table
       ([final_status.json]) from the same pass. *)
    let pkg_outcomes =
      List.map2 (fun (dag_node : Day11_opam_layer.Build.t) res ->
        let is_doc = match node_kind dag_node with
          | Day11_doc.Generate.Build | Tool -> false
          | Compile | Doc_all | Link -> true in
        let ok = Result.is_ok res in
        let cascaded =
          (not ok)
          && List.exists (fun (d : Day11_opam_layer.Build.t) -> not (dep_ok d.hash))
               dag_node.deps
        in
        (OpamPackage.to_string dag_node.pkg,
         { Day11_lib.Status_index.is_doc;
           blessed = node_blessed dag_node;
           ok; cascaded }))
        all_dag_nodes results
    in
    let outcomes = List.map snd pkg_outcomes in
    let scanned =
      List.sort_uniq compare (List.map fst pkg_outcomes) |> List.length
    in
    let status =
      Day11_lib.Status_index.of_outcomes
        ~run_id:(Day11_lib.Run_log.get_id run_log) ~scanned outcomes
    in
    Day11_lib.Status_index.write ~dir:snapshot_dir status;
    (* Run complete (guarded above), so write the blessed-package status
       table that drives the diff views. Reused below for the package
       metric — it's the per-blessed-package collapse (one entry each). *)
    let final = Day11_lib.Status_index.final_status_of_outcomes pkg_outcomes in
    Day11_lib.Status_index.write_final_status ~dir:snapshot_dir final;
    let sum = List.fold_left (fun acc (_, n) -> acc + n) 0 in
    Metrics.set_status
      ~profile:profile.name
      ~blessed:(sum status.Day11_lib.Status_index.blessed_totals)
      ~non_blessed:(sum status.non_blessed_totals)
      ~scanned:status.scanned;
    (* Node-level layer accounting: (side, result) tally over every plan
       node. [side] splits build/doc/tool; [result] is success, cascade
       (a dep failed so this never ran), or a genuine failure. Sums to the
       total layer count. *)
    let layer_counts =
      let tbl : (string * string, int) Hashtbl.t = Hashtbl.create 9 in
      List.iter2 (fun (dn : Day11_opam_layer.Build.t) res ->
        let side = match node_kind dn with
          | Day11_doc.Generate.Build -> "build"
          | Tool -> "tool"
          | Compile | Doc_all | Link -> "doc" in
        let result =
          if Result.is_ok res then "success"
          else if List.exists
                    (fun (d : Day11_opam_layer.Build.t) -> not (dep_ok d.hash))
                    dn.deps
          then "cascade" else "failure" in
        let key = (side, result) in
        Hashtbl.replace tbl key
          (1 + (try Hashtbl.find tbl key with Not_found -> 0)))
        all_dag_nodes results;
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
    in
    Metrics.set_layers ~profile:profile.name layer_counts;
    (* Package-level accounting. [final] holds one collapsed status per
       blessed package, so its length is the blessed-package count and the
       doc_success entries are the successes; everything else there is a
       failure (cascade / build / doc). [not_documentable] is every scanned
       package with no blessed doc node = scanned − blessed. [solver_failure]
       is solve failures that never made it into the plan (a handful solve
       only as another package's transitive dep — those are already counted
       in the plan, so excluding them keeps the buckets disjoint). *)
    let blessed_doc_success =
      List.length (List.filter (fun (_, s) -> s = "doc_success") final) in
    let blessed_doc_failure = List.length final - blessed_doc_success in
    let not_documentable = status.scanned - List.length final in
    let solver_failure =
      let in_plan = Hashtbl.create (List.length pkg_outcomes) in
      List.iter (fun (p, _) -> Hashtbl.replace in_plan p ()) pkg_outcomes;
      Day11_solver.read_solve_failures ~snapshot_dir
      |> List.filter (fun p -> not (Hashtbl.mem in_plan p))
      |> List.length
    in
    Metrics.set_packages
      ~profile:profile.name
      ~solver_failure ~not_documentable
      ~blessed_doc_success ~blessed_doc_failure;
    ()
  in
  (* Manual epoch promotion: a Dangerous OCurrent node per profile that,
     once confirmed in the web UI, swaps [html-live] to the freshly-built
     epoch. A sibling [Epoch_gc] node reclaims old epoch dirs as a
     separate, independently-confirmed action (deleting a multi-million-
     file epoch tree is too slow to bundle into the promote click).
     Independent of [builds] so a promote can be triggered whenever, while
     the previous epoch keeps serving until then. Only when this profile
     actually emits docs. *)
  match doc_plan with
  | None -> builds
  | Some plan ->
    Current.all
      [ builds;
        Epoch_promote.promote ~base_dir:plan.epoch_base
          ~epoch_hash:plan.epoch_hash;
        Epoch_gc.gc ~base_dir:plan.epoch_base
          ~epoch_hash:plan.epoch_hash ]

(* Fan out across profiles: each profile gets its own sub-pipeline
   composed under [Current.all]. Profiles with the same os_dir share
   the layer cache automatically, so overlapping package builds
   dedupe at the Day11_prep/OCurrent level.

   Each profile's [Profile_ctx.t] is itself a [Current.t] built from
   live [Current_git] polling of its [opam_repositories] — so changes
   to any repo (remote or local) invalidate the solver cache. See
   [Day11_profile_ctx_loader]. *)
let v ~config ~eio_env ~cache_dir ~profiles ~remote_commits
    ?cpu_slots () =
  let sub_pipelines =
    List.map (fun profile ->
      let resolved =
        Day11_profile_ctx_loader.resolve ~remote_commits
          ~cache_dir profile in
      v_for_profile ~config ~eio_env ~cache_dir ?cpu_slots
        ~tracking_commits:resolved.tracking_commits
        resolved.ctx
    ) profiles
  in
  Current.all sub_pipelines
