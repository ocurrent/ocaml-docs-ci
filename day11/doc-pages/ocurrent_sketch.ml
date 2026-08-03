(* SKETCH: day11 as an OCurrent pipeline

   This is not compilable code — it's a design sketch showing how
   day11's libraries would map onto OCurrent's abstractions.

   The key insight: day11's layer hashes are deterministic from
   inputs, so they serve as OCurrent cache keys directly. *)

open Current.Syntax

(* ── Cache modules ─────────────────────────────────────────────── *)

(* Each layer type gets a Current_cache.BUILDER that:
   - Uses the pre-computed layer hash as the cache key
   - Checks for the layer on disk before building
   - Calls the clean Doc_build/Build_layer primitive to do the work *)

module Build_cache = Current_cache.Make (struct
  type t = {
    env : Eio_unix.Stdenv.base;
    benv : Day11_opam_build.Types.build_env;
  }

  module Key = struct
    type t = { pkg : OpamPackage.t; hash : string }
    let digest t = t.hash
  end

  module Value = struct
    type t = Fpath.t  (* layer dir *)
    let marshal p = Fpath.to_string p
    let unmarshal s = Fpath.v s
  end

  let id = "day11-build"

  let pp f key = Fmt.pf f "build %s" (OpamPackage.to_string key.Key.pkg)

  let auto_cancel = false

  let build ctx job key =
    Current.Job.log job "Building %s" (OpamPackage.to_string key.pkg);
    let layer = Day11_layer.Layer.of_hash ~os_dir:ctx.benv.os_dir key.hash in
    if Day11_layer.Layer.exists layer then
      Lwt.return_ok (Day11_layer.Layer.dir layer)
    else
      (* Call the build primitive *)
      match Day11_opam_build.Build_layer.build ctx.env ctx.benv
              (* ... build args ... *)
              () with
      | Day11_opam_build.Types.Success _bl -> Lwt.return_ok layer_dir
      | _ -> Lwt.return_error (`Msg "build failed")
end)

module Compile_cache = Current_cache.Make (struct
  type t = {
    env : Eio_unix.Stdenv.base;
    benv : Day11_opam_build.Types.build_env;
    config : Day11_doc.Doc_build.doc_config;
  }

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer : Fpath.t;
      dep_compile_layers : Fpath.t list;
    }
    let digest t = t.hash  (* deterministic from inputs *)
  end

  module Value = struct
    type t = Fpath.t
    let marshal p = Fpath.to_string p
    let unmarshal s = Fpath.v s
  end

  let id = "day11-compile"
  let pp f key = Fmt.pf f "compile %s" (OpamPackage.to_string key.Key.pkg)
  let auto_cancel = false

  let build ctx _job key =
    match Day11_doc.Doc_build.compile ctx.env ctx.benv
            ~config:ctx.config
            ~build_layer:key.build_layer
            ~dep_compile_layers:key.dep_compile_layers
            ~hash:key.hash key.pkg with
    | Ok layer_dir -> Lwt.return_ok layer_dir
    | Error msg -> Lwt.return_error (`Msg msg)
end)

module Link_cache = Current_cache.Make (struct
  type t = {
    env : Eio_unix.Stdenv.base;
    benv : Day11_opam_build.Types.build_env;
    config : Day11_doc.Doc_build.doc_config;
    html_dir : Fpath.t;
  }

  module Key = struct
    type t = {
      pkg : OpamPackage.t;
      hash : string;
      build_layer : Fpath.t;
      compile_layer : Fpath.t;
      dep_compile_layers : Fpath.t list;
    }
    let digest t = t.hash
  end

  module Value = Current.Unit

  let id = "day11-link"
  let pp f key = Fmt.pf f "link %s" (OpamPackage.to_string key.Key.pkg)
  let auto_cancel = false

  let build ctx _job key =
    match Day11_doc.Doc_build.link ctx.env ctx.benv
            ~config:ctx.config
            ~build_layer:key.build_layer
            ~compile_layer:key.compile_layer
            ~dep_compile_layers:key.dep_compile_layers
            ~html_dir:ctx.html_dir
            ~hash:key.hash key.pkg with
    | Ok () -> Lwt.return_ok ()
    | Error msg -> Lwt.return_error (`Msg msg)
end)

(* ── Pipeline ──────────────────────────────────────────────────── *)

(* The pipeline wires together:
   1. Track opam-repo for changes
   2. Solve all packages
   3. Build all packages (fan out, DAG from deps)
   4. Compile docs (fan out, DAG from compile deps)
   5. Link docs (fan out, DAG from doc deps)

   OCurrent handles scheduling, caching, parallelism, and the web UI. *)

let pipeline ~opam_repo ~config () =
  (* 1. Track opam-repository HEAD *)
  let repo = Current_git.Local.head_commit (Current.return opam_repo) in

  (* 2. Solve — produces a map from package to solve result *)
  let solutions =
    let+ commit = repo in
    let packages, repos_with_shas, env =
      Day11_opam.Git_packages.of_repositories [ (opam_repo, None) ] in
    (* Solve all packages in the small universe *)
    let targets = Day11_batch.Targets.resolve ~small:true packages None in
    List.filter_map (fun target ->
      match Day11_solver.Solve.solve ~packages ~env target with
      | Ok result -> Some (target, result)
      | Error _ -> None
    ) targets
  in

  (* 3. Build — each package becomes a Current.t of its build layer path *)
  let build_layers =
    let+ solutions = solutions in
    (* Compute the DAG and build all layers *)
    let cache = Day11_opam_build.Hash_cache.create () in
    let nodes = Day11_opam_build.Dag.build_dag cache
      ~base_hash:"..." solutions in
    (* Each node's hash is deterministic — use as cache key *)
    List.map (fun (node : Day11_opam_layer.Build.t) ->
      (node.pkg, node.hash, Build_cache.get ctx { pkg = node.pkg; hash = node.hash })
    ) nodes
  in

  (* 4. Compile — each package's compile depends on its deps' compiles *)
  (* This is where the DAG emerges from Current.t dependencies *)
  let compile_layers =
    let+ build_layers = build_layers in
    List.map (fun (pkg, hash, build_layer) ->
      let dep_compiles = (* look up deps' compile layers *) in
      let+ build = build_layer
      and+ deps = Current.list_seq dep_compiles in
      Compile_cache.get ctx {
        pkg; hash = compile_hash;
        build_layer = build;
        dep_compile_layers = deps;
      }
    ) build_layers
  in

  (* 5. Link — depends on compile layer + doc-dep compile layers *)
  let _links =
    let+ compile_layers = compile_layers in
    List.map (fun (pkg, compile_layer, doc_dep_compiles) ->
      let+ compile = compile_layer
      and+ deps = Current.list_seq doc_dep_compiles in
      Link_cache.get ctx {
        pkg; hash = link_hash;
        build_layer = (* ... *);
        compile_layer = compile;
        dep_compile_layers = deps;
      }
    ) compile_layers
  in

  Current.return ()


(* ── Notes ─────────────────────────────────────────────────────── *)

(* What OCurrent gives us for free:
   - Reactive: re-runs when opam-repo changes
   - Incremental: only rebuilds what changed (via cache key = layer hash)
   - Parallel: independent Current.t values run concurrently
   - Web UI: shows pipeline status, per-package progress
   - Error propagation: failed deps cascade automatically

   What day11 gives OCurrent:
   - Clean build primitives (Doc_build.compile/link/doc_all)
   - Content-addressed layer caching (layer hash = cache key)
   - Overlayfs stacking for efficient dep assembly
   - Deterministic hash computation (no post-hoc hash extraction)

   The key architectural win: OCurrent's cache digest IS the layer hash.
   No separate caching logic needed — the two systems align perfectly.

   What's NOT needed from day11 in this model:
   - dag_executor.ml (OCurrent does scheduling)
   - Profile/Snapshot (OCurrent pipeline IS the profile)
   - cmd_batch.ml (OCurrent replaces it)
   - Summary/Status_index (OCurrent web UI replaces it)

   What IS needed:
   - Day11_solver.Solve.solve
   - Day11_opam_build.Build_layer.build (or a cleaner primitive)
   - Day11_doc.Doc_build.compile/link/doc_all
   - Day11_layer.* (on-disk format)
   - Day11_container.* (runc execution)
   - Day11_runner.Run_in_layers.run (overlayfs assembly)
*)
