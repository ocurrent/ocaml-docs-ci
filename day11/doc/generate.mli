(** Doc generation orchestration.

    Two-pass process:
    - {b Compile}: for each package with installed libraries, run
      [odoc_driver_voodoo --actions compile-only] in a container with
      the package's build layer. Produces [.odoc] files.
    - {b Link}: for each compiled package, run
      [odoc_driver_voodoo --actions link-and-gen] with the compile
      layers of all packages in the same solution stacked for
      cross-referencing. Produces HTML.

    Tool binaries (odoc, odoc-md, odoc_driver_voodoo, sherlodoc) are
    bind-mounted from the tool layers at [/home/opam/doc-tools/bin/],
    keeping tool libraries isolated from documented packages.

    The per-compiler odoc binary is selected based on which compiler
    appears in each package's solution.

    Tool builds are included as nodes in the unified DAG, so they
    run in parallel with regular package builds instead of blocking. *)

val find_compiler :
  Day11_solution.Deps.t -> OpamPackage.t option
(** [find_compiler solution] returns the concrete compiler package
    ([ocaml-base-compiler], [ocaml-variants], or [ocaml-system])
    from [solution], or [None] if none is found. *)

val unique_compilers :
  (OpamPackage.t * Day11_solution.Solve_result.t) list ->
  OpamPackage.t list
(** Extract unique concrete compiler packages from solutions. *)

val run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  np:int ->
  os_dir:Fpath.t ->
  html_dir:Fpath.t ->
  cache:Day11_opam_build.Hash_cache.t ->
  base_hash:string ->
  driver_tool:Day11_opam_layer.Tool.t ->
  odoc_tools:(OpamPackage.t * Day11_opam_layer.Tool.t) list ->
  tool_source_dirs:string OpamPackage.Name.Map.t ->
  mounts:Day11_container.Mount.t list ->
  run_log:Day11_lib.Run_log.t ->
  build_one:(Day11_opam_layer.Build.t -> bool) ->
  ?on_pkg_complete:(Day11_opam_layer.Build.t ->
                    cached:bool -> success:bool -> unit) ->
  ?on_doc_complete:(Day11_opam_layer.Build.t ->
                    cached:bool -> blessed:bool -> universe:string ->
                    success:bool -> unit) ->
  ?snapshot_dir:Fpath.t ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Solve_result.t) list ->
  blessing_maps:(OpamPackage.t * bool OpamPackage.Map.t) list ->
  unit ->
  int * int
(** Run the unified build+doc DAG with pre-resolved tools.
    Returns [(doc_count, html_file_count)]. Uses {!Day11_opam_build.Dag_executor}
    for parallel execution with priority ordering (link > compile > tool > build). *)

(** {1 Planned doc DAG}

    [plan_doc_dag] constructs the doc DAG nodes without executing them.
    This is useful for external executors (like OCurrent) that want to
    create their own scheduling nodes from the DAG. *)

type node_kind = Build | Tool | Compile | Doc_all | Link

(** {1 Internal planning — exposed for white-box testing}

    [build_internal_plan] is the pure core of doc-DAG construction: given
    the build nodes and solutions (plus the resolved doc tools) it derives
    the per-universe compile/doc-all/link nodes, their layer hashes, and
    each node's blessing. It runs no containers, so it is unit-testable on
    any platform — see [test/test_generate_plan.ml]. *)

type doc_node = {
  build_node : Day11_opam_layer.Build.t;
  kind : node_kind;
  layer : Day11_opam_layer.Build.t;
  doc_deps : doc_node list;
  compile_layer : Day11_opam_layer.Build.t option;
  compiler : OpamPackage.t option;
  odoc_tool : Day11_opam_layer.Tool.t option;
  universe : string;
  blessed : bool;
}

type internal_plan = {
  all_nodes : Day11_opam_layer.Build.t list;
  meta : (string, doc_node) Hashtbl.t;
      (** Keyed by each node's own layer hash; the single source of truth
          for a node's kind, universe and blessing. *)
  driver_tool : Day11_opam_layer.Tool.t;
}

val build_internal_plan :
  os_dir:Fpath.t ->
  cache:Day11_opam_build.Hash_cache.t ->
  base_hash:string ->
  driver_tool:Day11_opam_layer.Tool.t ->
  odoc_tools:(OpamPackage.t * Day11_opam_layer.Tool.t) list ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Solve_result.t) list ->
  internal_plan

type doc_plan = {
  all_nodes : Day11_opam_layer.Build.t list;
  (** All nodes in the unified DAG (build + tool + doc). *)
  node_kind : Day11_opam_layer.Build.t -> node_kind;
  (** Classify a node by its phase. *)
  node_blessed : Day11_opam_layer.Build.t -> bool;
  (** Whether the plan blessed this node (its universe is the package's
      canonical one). *)
  build_one : sw:Eio.Switch.t -> Eio_unix.Stdenv.base ->
    Day11_opam_layer.Build.t -> bool;
  (** Callback to build a single node. Takes a per-call Eio switch and
      the env; handles dispatch to the correct phase (build, tool,
      compile, link, doc-all). The per-call switch lets external
      executors (e.g. OCurrent nodes) bound the lifetime of spawned
      subprocesses to each build attempt. *)
  epoch_hash : string;
  (** Epoch hash for this doc run (see {!Day11_lib.Epoch.compute}). The
      doc stages write HTML into [epoch_base/epoch-<epoch_hash>/html]. *)
  epoch_base : Fpath.t;
  (** The profile's HTML base dir, under which epoch dirs and the
      [html-live] symlink live. *)
}

val plan_doc_dag :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_batch.Profile_ctx.t ->
  mounts:Day11_container.Mount.t list ->
  build_one:(Day11_opam_layer.Build.t -> bool) ->
  ?on_pkg_complete:(Day11_opam_layer.Build.t -> success:bool -> unit) ->
  ?on_doc_complete:(Day11_opam_layer.Build.t ->
                    blessed:bool -> universe:string -> success:bool -> unit) ->
  ?snapshot_dir:Fpath.t ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Solve_result.t) list ->
  blessing_maps:(OpamPackage.t * bool OpamPackage.Map.t) list ->
  unit ->
  doc_plan option
(** Plan the doc DAG: solve for tools, construct compile/link/doc-all
    nodes with deterministic hashes, and return the unified DAG.
    Returns [None] if tool solving fails. All profile-derived inputs
    (git packages, repos, odoc repo, driver compiler, build env, hash
    cache) come from [ctx]. When [snapshot_dir] is given, the planned
    DAG is written to [<snapshot_dir>/dag.json] for offline cascade
    attribution (see {!Dag_marshal}). *)

val build_tools_and_run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_batch.Profile_ctx.t ->
  np:int ->
  mounts:Day11_container.Mount.t list ->
  build_one:(Day11_opam_layer.Build.t -> bool) ->
  ?on_pkg_complete:(Day11_opam_layer.Build.t ->
                    cached:bool -> success:bool -> unit) ->
  ?on_doc_complete:(Day11_opam_layer.Build.t ->
                    cached:bool -> blessed:bool -> universe:string ->
                    success:bool -> unit) ->
  ?snapshot_dir:Fpath.t ->
  run_log:Day11_lib.Run_log.t ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Solve_result.t) list ->
  blessing_maps:(OpamPackage.t * bool OpamPackage.Map.t) list ->
  unit ->
  unit
(** Plan doc tools (driver + per-compiler odoc) and run a unified DAG
    that builds packages, tools, and docs in parallel. Profile-derived
    state comes from [ctx]: when [ctx.driver_compiler] is [None] the
    latest compiler from [solutions] is used; odoc is solved once per
    unique compiler; when [ctx.profile.odoc_repo] is set, odoc
    packages are pinned from that local checkout. *)
