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
