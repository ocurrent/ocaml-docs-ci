(** Parallel solving via solver_worker subprocesses.

    Spawns solver_worker processes to solve packages in parallel. No in-process
    solver dependency — all solving happens out-of-process.

    Subprocess execution goes through {!Day11_sys.Run}, so the fork helper is
    reused. Each worker's stdout is redirected to a per-worker JSONL file;
    results are parsed after the worker exits. The worker fibers are bounded by
    [np], supervised by the supplied switch — cancelling [sw] aborts all
    in-flight workers. *)

val solve_many :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  ?pin_dirs:string list ->
  ?constraints:OpamPackage.t list ->
  ?extra_targets:OpamPackage.t list ->
  ?doc:bool ->
  ?pin_target:bool ->
  ?ocaml_version:OpamPackage.t ->
  ?on_progress:(done_count:int -> total:int -> unit) ->
  np:int ->
  repos:(string * string) list ->
  OpamPackage.t list ->
  (OpamPackage.t
  * (Day11_solution.Solve_result.t, string * OpamPackage.Name.Set.t) result)
  list
(** [solve_many ~sw env ?pin_dirs ?constraints ?doc ?ocaml_version ?on_progress
     ~np ~repos targets] solves all [targets] in parallel by spawning up to [np]
    solver_worker fibers that each invoke [day11-solver-worker].

    [on_progress] is called after each worker finishes with the total count of
    solved targets (across all workers). [repos] is a list of
    [(repo_path, commit_sha)] pairs. [pin_dirs] are directories of [.opam] files
    pinned at version [dev]. [constraints] pins packages at exact versions.
    [doc] controls whether doc dependencies are included (default [true]).
    [pin_target] controls whether each target's exact version is forced via an
    [=] constraint (default [true], for back-compat). Pass [false] in
    latest-version profile mode so the solver can choose a different version
    (e.g. an oxcaml [+ox] variant) when the nominal latest doesn't fit the rest
    of the universe — see {!Day11_solver.Solve.solve}. Workers run under [sw];
    cancelling [sw] terminates them. *)
