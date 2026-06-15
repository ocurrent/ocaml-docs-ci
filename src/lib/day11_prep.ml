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

(* Per-kind Op modules so job filenames include the node kind
   (e.g. "day11-build-XXXXXX.log" instead of "day11-node-XXXXXX.log"). *)

module type LABEL = sig val label : string end

module Make_op (L : LABEL) = struct
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
    }
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

  let label = L.label
  let id = "day11-" ^ L.label

  (* Short layer hash, matching the 12-char layer-dir naming, so a job
     can be tied to its on-disk layer ([<os_dir>/<hash>]) at a glance. *)
  let short_hash h = String.sub h 0 (min 12 (String.length h))

  (* Include the layer hash in the job's display name so it shows in the
     OCurrent "New job:" line and the /jobs dashboard, not just in the
     job's body log. *)
  let pp f (key : Key.t) =
    Fmt.pf f "%s %s (%s)" label (OpamPackage.to_string key.pkg)
      (short_hash key.hash)

  let auto_cancel = false

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
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

module Op_build   = Make_op (struct let label = "build" end)
module Op_tool    = Make_op (struct let label = "tool" end)
module Op_compile = Make_op (struct let label = "compile" end)
module Op_doc     = Make_op (struct let label = "doc" end)
module Op_link    = Make_op (struct let label = "link" end)

module Cache_build   = Current_cache.Make (Op_build)
module Cache_tool    = Current_cache.Make (Op_tool)
module Cache_compile = Current_cache.Make (Op_compile)
module Cache_doc     = Current_cache.Make (Op_doc)
module Cache_link    = Current_cache.Make (Op_link)

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
    match label with
    | "build" ->
      Cache_build.get { os_dir; dag_node; dispatch; env; pool; profile_name }
        Op_build.Key.{ hash = dag_node.hash; pkg = dag_node.pkg }
      |> Current.Primitive.map_result
        (Result.map (fun v ->
          (v.Op_build.Value.hash, Fpath.v v.Op_build.Value.layer_dir)))
    | "tool" ->
      Cache_tool.get { os_dir; dag_node; dispatch; env; pool; profile_name }
        Op_tool.Key.{ hash = dag_node.hash; pkg = dag_node.pkg }
      |> Current.Primitive.map_result
        (Result.map (fun v ->
          (v.Op_tool.Value.hash, Fpath.v v.Op_tool.Value.layer_dir)))
    | "compile" ->
      Cache_compile.get { os_dir; dag_node; dispatch; env; pool; profile_name }
        Op_compile.Key.{ hash = dag_node.hash; pkg = dag_node.pkg }
      |> Current.Primitive.map_result
        (Result.map (fun v ->
          (v.Op_compile.Value.hash, Fpath.v v.Op_compile.Value.layer_dir)))
    | "doc" ->
      Cache_doc.get { os_dir; dag_node; dispatch; env; pool; profile_name }
        Op_doc.Key.{ hash = dag_node.hash; pkg = dag_node.pkg }
      |> Current.Primitive.map_result
        (Result.map (fun v ->
          (v.Op_doc.Value.hash, Fpath.v v.Op_doc.Value.layer_dir)))
    | "link" ->
      Cache_link.get { os_dir; dag_node; dispatch; env; pool; profile_name }
        Op_link.Key.{ hash = dag_node.hash; pkg = dag_node.pkg }
      |> Current.Primitive.map_result
        (Result.map (fun v ->
          (v.Op_link.Value.hash, Fpath.v v.Op_link.Value.layer_dir)))
    | l -> Fmt.failwith "Unknown node label: %s" l
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
