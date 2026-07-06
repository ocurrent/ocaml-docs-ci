(** Day11-based build/doc nodes for the OCurrent pipeline.

    Each DAG node (build, tool, compile, link, doc-all) becomes an
    OCurrent component with its own job log, visible in the web UI.

    Uses Current_cache for job tracking but delegates all actual
    caching to day11's content-addressed layer store. *)

type t = {
  pkg : OpamPackage.t;
  build_hash : string;
  layer_dir : Fpath.t;
}
(* The value threaded between nodes is deliberately small and stable.
   [build_hash] is a deterministic function of the node's inputs, so a
   node's value never changes once computed; combined with the [~eq]
   cutoff in [run_node] this stops a completed node from re-triggering
   its whole downstream cone on every propagation.

   It used to also carry [all_layer_dirs] — this node's dir plus the
   full transitive closure of its deps' dirs — rebuilt (hashtable dedup
   + fresh list) at every node on every evaluation. That was both the
   per-event O(cone) cost and the bulk of the heap, and it was dead:
   nothing outside this module read it, and dispatch recomputes the
   overlay-stack dirs from the static DAG node itself
   ([Container_backend.collect_transitive_dep_dirs]). *)

let pkg t = t.pkg
let build_hash t = t.build_hash
let layer_dir t = t.layer_dir

let has_documentable_libs t =
  Day11_doc.Doc_build.has_documentable_libs t.layer_dir

let pp f t =
  Fmt.pf f "day11-build(%s, %s)"
    (OpamPackage.to_string t.pkg)
    (String.sub t.build_hash 0 (min 12 (String.length t.build_hash)))

let compare a b = String.compare a.build_hash b.build_hash

(* ── OCurrent cache builder ──────────────────────────────────────

   A single builder handles all node types (build, tool, compile,
   link, doc-all). The [dispatch] callback determines what runs.
   Current_cache provides the job/log infrastructure; day11's disk
   cache provides the real caching. *)

(* One op for every node kind. The unit of caching is the content-
   addressed layer, identified by its [hash] ALONE — the node "kind"
   (build/tool/compile/doc/link) is the layer's role at a DAG position,
   not part of its identity. Keying the cache by [hash] (not by kind)
   gives each layer exactly one cache entry, so a layer reached as both
   a build-dep and a tool-dep is deduplicated by Current_cache's own
   in-flight tracking — the two dispatches can no longer race to produce
   the same layer dir, which was the source of split [ok=1]/[ok=0] rows
   for a single layer. The kind rides on the [Key] purely for display in
   job logs. *)
module Op = struct
  type t = {
    os_dir : Fpath.t;
    dag_node : Day11_opam_layer.Build.t;
    dispatch : Eio_unix.Stdenv.base -> Day11_opam_layer.Build.t -> bool;
    env : Eio_unix.Stdenv.base;
    pool : unit Current.Pool.t;
    profile_name : string;
      (* Tag job logs with the profile that scheduled the run. A
         shared layer hash can be scheduled from more than one
         profile; the last writer wins here, which is fine for
         log-line attribution. *)
  }

  module Key = struct
    type t = {
      hash : string;
      pkg : OpamPackage.t;
      label : string;  (* node kind — display only, deliberately NOT in [digest] *)
    }
    (* Identity is the content hash alone, so two nodes of different kind
       that resolve to the same layer share one cache entry. *)
    let digest t = t.hash
  end

  module Value = struct
    type t = {
      pkg : string;
      hash : string;
      layer_dir : string;
    }
    [@@deriving yojson]

    let marshal t = Yojson.Safe.to_string (to_yojson t)
    let unmarshal s = of_yojson (Yojson.Safe.from_string s) |> Result.get_ok
  end

  let id = "day11-node"

  (* Short layer hash, matching the 12-char layer-dir naming, so a job
     can be tied to its on-disk layer ([<os_dir>/<hash>]) at a glance. *)
  let short_hash h = String.sub h 0 (min 12 (String.length h))

  (* Include the layer hash in the job's display name so it shows in the
     OCurrent "New job:" line and the /jobs dashboard, not just in the
     job's body log. *)
  let pp f (key : Key.t) =
    Fmt.pf f "%s %s (%s)" key.label (OpamPackage.to_string key.pkg)
      (short_hash key.hash)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let label = key.label in
    let* () = Current.Job.start job ~pool:ctx.pool ~level:Current.Level.Average in
    Current.Job.log job "[profile %s] %s %s" ctx.profile_name
      label (OpamPackage.to_string key.pkg);
    let layer = Day11_layer.Layer.of_hash ~os_dir:ctx.os_dir key.hash in
    Lwt_eio.run_eio @@ fun () ->
    let cached_ok = match Day11_layer.Meta.load ctx.env (Day11_layer.Layer.meta_path layer) with
      | Ok meta when meta.exit_status = 0 -> true
      | Ok _ ->
        Current.Job.log job "Clearing failed layer %s" key.hash;
        ignore (Bos.OS.Dir.delete ~recurse:true (Day11_layer.Layer.dir layer));
        false
      | Error _ -> false
    in
    if cached_ok then begin
      (* Hits are high-volume on large profiles — debug level keeps
         the default log focused on genuine work. Bump via
         [--verbosity debug] to see them. *)
      Log.debug (fun f -> f "[%s] cache hit: %s %s %s"
        ctx.profile_name label
        (OpamPackage.to_string key.pkg)
        (short_hash key.hash));
      Current.Job.log job "Cached: %s %s (%s)" label
        (OpamPackage.to_string key.pkg)
        (short_hash key.hash);
      Ok Value.{
        pkg = OpamPackage.to_string key.pkg;
        hash = key.hash;
        layer_dir = Fpath.to_string (Day11_layer.Layer.dir layer);
      }
    end else begin
      Log.info (fun f -> f "[%s] cache miss: %s %s %s"
        ctx.profile_name label
        (OpamPackage.to_string key.pkg)
        (short_hash key.hash));
      Current.Job.log job "%s %s (%s)" label
        (OpamPackage.to_string key.pkg)
        (short_hash key.hash);
      let success = ctx.dispatch ctx.env ctx.dag_node in
      (match Bos.OS.File.read (Day11_layer.Layer.log_path layer) with
       | Ok contents -> Current.Job.write job contents
       | Error _ -> ());
      if success then begin
        (match Day11_layer.Meta.load ctx.env (Day11_layer.Layer.meta_path layer) with
         | Ok meta ->
           let tf name = Day11_layer.Meta.timing_field name meta.timing in
           Current.Job.log job "OK: %s %s (runc: %.1fs, disk: %dKB)"
             label (OpamPackage.to_string key.pkg)
             (tf "runc_run") (meta.disk_usage / 1024)
         | Error _ ->
           Current.Job.log job "OK: %s %s" label
             (OpamPackage.to_string key.pkg));
        Ok Value.{
          pkg = OpamPackage.to_string key.pkg;
          hash = key.hash;
          layer_dir = Fpath.to_string (Day11_layer.Layer.dir layer);
        }
      end else begin
        Current.Job.log job "FAILED: %s %s" label
          (OpamPackage.to_string key.pkg);
        Error (`Msg (Printf.sprintf "%s failed: %s" label
          (OpamPackage.to_string key.pkg)))
      end
    end
end

module Cache = Current_cache.Make (Op)

(* Reconcile the OCurrent cache against on-disk layers.

   A [day11-node] success only records "this layer was built at some
   point": the op is keyed by layer hash alone and Current_cache never
   re-checks whether the layer dir still exists. Layers get removed
   out-of-band — the layer GC prunes by last-used, an os_dir migration
   moves them, cleanup deletes them — and the stale success then makes
   OCurrent skip the rebuild indefinitely, so downstream renders the
   node as permanently "pending" (it never re-dispatches, so
   [layer_status.jsonl] never repopulates).

   [reconcile_cache] walks the cached successes and, for any whose layer
   dir is gone, calls [Cache.invalidate] — which sets [rebuild=1] in the
   cache db (persisted) so the next evaluation re-dispatches and rebuilds
   it. Runs in-process so both the db and any live in-memory instance are
   updated. Cheap: one query plus a [stat] per cached success. Returns
   the number invalidated. *)
let reconcile_cache () =
  let entries = Current_cache.Db.query ~op:Op.id ~ok:true () in
  List.fold_left (fun n (e : Current_cache.Db.entry) ->
    match e.outcome with
    | Error _ -> n
    | Ok payload ->
      match (try Some (Op.Value.unmarshal payload) with _ -> None) with
      | None -> n
      | Some (v : Op.Value.t) ->
        if Sys.file_exists (Filename.concat v.layer_dir "layer.json") then n
        else begin
          (match OpamPackage.of_string_opt v.pkg with
           | Some pkg ->
             Cache.invalidate Op.Key.{ hash = v.hash; pkg; label = "" }
           | None -> ());
          n + 1
        end
  ) 0 entries

(* ── Public interface ──────────────────────────────────────────── *)

(** Run a DAG node as an OCurrent component with job logs.
    [dag_node] is the original DAG node with full deps and universe.
    [dispatch] is called with the original node to execute it.
    [deps] are OCurrent dependencies that must complete first. *)
let run_node ~env ~os_dir ~pool ~dispatch ~label ~profile_name
    ~(dag_node : Day11_opam_layer.Build.t) ~deps () : t Current.t =
  let open Current.Syntax in
  let node =
    (* [deps] is now a pure ordering/cascade gate (unit), not a value to
       read. A dep in [Error] still short-circuits the [let>] body so
       dispatch is never invoked and this node cascades to [Error]; an
       [Error -> Ok] transition when a dep recovers still re-fires this
       node, so rerunning a failed layer rebuilds everything below it. *)
    (Current.component "[%s] %s %s" profile_name label
      (OpamPackage.to_string dag_node.pkg)
  |>
  let> () = deps in
  let result =
    Cache.get { os_dir; dag_node; dispatch; env; pool; profile_name }
      Op.Key.{ hash = dag_node.hash; pkg = dag_node.pkg; label }
    |> Current.Primitive.map_result
      (Result.map (fun v ->
        (v.Op.Value.hash, Fpath.v v.Op.Value.layer_dir)))
  in
  result
  |> Current.Primitive.map_result
       (Result.map (fun (hash, own_dir) ->
         { pkg = dag_node.pkg;
           build_hash = hash;
           layer_dir = own_dir })))
  in
  (* Cut off propagation when this node's value is unchanged: a node
     re-emitting the same [build_hash] (the common case under churn)
     does not re-trigger its dependents. [Dyn.equal] treats Error/Ok
     transitions as unequal regardless of this [eq], so failures and
     recoveries always propagate. *)
  Current.cutoff ~eq:(fun a b -> String.equal a.build_hash b.build_hash) node
