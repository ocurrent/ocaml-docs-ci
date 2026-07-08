(** Prometheus metrics for the docs pipeline.

    Each definition registers into {!Prometheus.CollectorRegistry.default}
    on first evaluation; the [/metrics] route (see
    {!Docs_ci_web.Metrics_page}) renders that registry in the text
    exposition format. The recording functions are called from the
    pipeline — build/doc completion callbacks and status regeneration in
    {!Docs_ci_pipelines.Docs}. Defining them here (in the app library,
    not day11) keeps the prometheus dependency out of the lower layers. *)

let namespace = "docs_ci"

(* Every metric carries a [profile] label so the profiles don't clobber
   one another — each has its own series in the [/metrics] output. *)

(* ── Pipeline counters ─────────────────────────────────────────── *)

(* Counters only ever increment, so they're meaningful with rate(). Both
   fire from callbacks that run only for nodes that actually executed —
   cache hits don't reach them — so these count real build/doc work. *)

let builds_total =
  Prometheus.Counter.v_labels ~label_names:[ "profile"; "result" ]
    ~help:"Package build nodes executed (cache hits excluded), by result."
    ~namespace ~subsystem:"pipeline" "builds_total"

let record_build ~profile ~success =
  Prometheus.Counter.inc_one
    (Prometheus.Counter.labels builds_total
       [ profile; (if success then "ok" else "fail") ])

let docs_total =
  Prometheus.Counter.v_labels ~label_names:[ "profile"; "result"; "blessed" ]
    ~help:"Doc nodes (compile/doc-all/link) executed, by result and blessing."
    ~namespace ~subsystem:"pipeline" "docs_total"

let record_doc ~profile ~success ~blessed =
  Prometheus.Counter.inc_one
    (Prometheus.Counter.labels docs_total
       [ profile;
         (if success then "ok" else "fail");
         (if blessed then "true" else "false") ])

(* ── Status gauges (the profile's latest completed snapshot) ───────

   Set from the completion-gated status step in {!Docs_ci_pipelines.Docs}
   — which only runs once every planned node has resolved — so each
   profile's gauge reflects its latest *completed* snapshot. *)

let gauge name help =
  Prometheus.Gauge.v_label ~label_name:"profile"
    ~help ~namespace ~subsystem:"status" name

let packages_blessed =
  gauge "packages_blessed"
    "Blessed build/doc outcomes in the profile's latest completed snapshot."

let packages_non_blessed =
  gauge "packages_non_blessed"
    "Non-blessed outcomes in the profile's latest completed snapshot."

let packages_scanned =
  gauge "packages_scanned"
    "Packages the plan covered in the profile's latest completed snapshot."

let set_status ~profile ~blessed ~non_blessed ~scanned =
  Prometheus.Gauge.set (packages_blessed profile) (float_of_int blessed);
  Prometheus.Gauge.set (packages_non_blessed profile) (float_of_int non_blessed);
  Prometheus.Gauge.set (packages_scanned profile) (float_of_int scanned)

(* ── Layer (node) accounting — the profile's latest completed snapshot ──

   One series per (side, result). [side] ∈ build | doc | tool; [result] ∈
   success | failure | cascade (a node that never ran because a dependency
   failed). These partition every plan node exactly once, so
   sum(status_layers{profile=P}) = total layers built for P, and folding
   [cascade] into [failure] (a PromQL sum over [result]) gives the plain
   success/failure split. Set from the same completion-gated status step as
   the gauges above. All nine combinations are written every time (0 when
   absent) so a category that empties out doesn't retain a stale value. *)
let layer_sides = [ "build"; "doc"; "tool" ]
let layer_results = [ "success"; "failure"; "cascade" ]

let status_layers =
  Prometheus.Gauge.v_labels ~label_names:[ "profile"; "side"; "result" ]
    ~help:"Plan nodes (layers) in the profile's latest completed snapshot, \
           by side (build/doc/tool) and result (success/failure/cascade). \
           Partitions all layers; sum for a total, fold cascade into \
           failure for the plain split."
    ~namespace ~subsystem:"status" "layers"

(* [counts] maps (side, result) -> n; missing pairs are recorded as 0. *)
let set_layers ~profile counts =
  List.iter (fun side ->
    List.iter (fun result ->
      let n = try List.assoc (side, result) counts with Not_found -> 0 in
      Prometheus.Gauge.set
        (Prometheus.Gauge.labels status_layers [ profile; side; result ])
        (float_of_int n))
      layer_results)
    layer_sides

(* ── Package accounting — the profile's latest completed snapshot ──

   One series per package [outcome]; every package.version the pipeline
   attempted lands in exactly one. So
   sum(status_packages{profile=P}) = total attempted, and dropping
   [solver_failure] gives [scanned] (the packages that solved). Blessing
   is per package here (its canonical universe collapsed to one status),
   unlike the node-level {!status_layers}. *)
let package_outcomes =
  [ "solver_failure"; "not_documentable";
    "blessed_doc_success"; "blessed_doc_failure" ]

let status_packages =
  Prometheus.Gauge.v_labels ~label_names:[ "profile"; "outcome" ]
    ~help:"Packages in the profile's latest completed snapshot by outcome: \
           solver_failure (never solved), not_documentable (solved, no libs \
           to document), blessed_doc_success / blessed_doc_failure (canonical \
           docs built / failed). Sum = attempted; without solver_failure = \
           scanned."
    ~namespace ~subsystem:"status" "packages"

let set_packages ~profile ~solver_failure ~not_documentable
    ~blessed_doc_success ~blessed_doc_failure =
  let by = [ "solver_failure", solver_failure;
             "not_documentable", not_documentable;
             "blessed_doc_success", blessed_doc_success;
             "blessed_doc_failure", blessed_doc_failure ] in
  List.iter (fun outcome ->
    let n = try List.assoc outcome by with Not_found -> 0 in
    Prometheus.Gauge.set
      (Prometheus.Gauge.labels status_packages [ profile; outcome ])
      (float_of_int n))
    package_outcomes

(* ── Host disk gauges (sampled periodically, host-level) ───────────
   Not per-profile: the root filesystem and the layer cache are shared
   across profiles. *)

let disk_gauge name help =
  Prometheus.Gauge.v ~help ~namespace ~subsystem:"disk" name

let disk_root_used_percent =
  disk_gauge "root_used_percent" "Root filesystem usage, percent (df /)."

let layers_total_bytes =
  disk_gauge "layers_total_bytes"
    "Total size of all build layers across every os_dir, summed from \
     each layer's disk_usage metadata (not by measuring)."

(* [root_percent] < 0 means the df sample failed; leave that gauge as-is
   rather than record a bogus value. *)
let set_disk ~root_percent ~layer_bytes =
  if root_percent >= 0 then
    Prometheus.Gauge.set disk_root_used_percent (float_of_int root_percent);
  Prometheus.Gauge.set layers_total_bytes (float_of_int layer_bytes)
