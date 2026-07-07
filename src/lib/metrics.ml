(** Prometheus metrics for the docs pipeline.

    Each definition registers into {!Prometheus.CollectorRegistry.default}
    on first evaluation; the [/metrics] route (see
    {!Docs_ci_web.Metrics_page}) renders that registry in the text
    exposition format. The recording functions are called from the
    pipeline — build/doc completion callbacks and status regeneration in
    {!Docs_ci_pipelines.Docs}. Defining them here (in the app library,
    not day11) keeps the prometheus dependency out of the lower layers. *)

let namespace = "docs_ci"

(* ── Pipeline counters ─────────────────────────────────────────── *)

(* Counters only ever increment, so they're meaningful with rate(). Both
   fire from callbacks that run only for nodes that actually executed —
   cache hits don't reach them — so these count real build/doc work. *)

let builds_total =
  Prometheus.Counter.v_label ~label_name:"result"
    ~help:"Package build nodes executed (cache hits excluded), by result."
    ~namespace ~subsystem:"pipeline" "builds_total"

let record_build ~success =
  Prometheus.Counter.inc_one (builds_total (if success then "ok" else "fail"))

let docs_total =
  Prometheus.Counter.v_labels ~label_names:[ "result"; "blessed" ]
    ~help:"Doc nodes (compile/doc-all/link) executed, by result and blessing."
    ~namespace ~subsystem:"pipeline" "docs_total"

let record_doc ~success ~blessed =
  Prometheus.Counter.inc_one
    (Prometheus.Counter.labels docs_total
       [ (if success then "ok" else "fail");
         (if blessed then "true" else "false") ])

(* ── Status gauges (set on each status regeneration) ───────────── *)

let gauge name help =
  Prometheus.Gauge.v ~help ~namespace ~subsystem:"status" name

let packages_blessed =
  gauge "packages_blessed"
    "Live-blessed build outcomes in the most recent status index."

let packages_non_blessed =
  gauge "packages_non_blessed"
    "Non-live build outcomes in the most recent status index."

let packages_scanned =
  gauge "packages_scanned"
    "Packages the plan covered in the most recent status regeneration."

let set_status ~blessed ~non_blessed ~scanned =
  Prometheus.Gauge.set packages_blessed (float_of_int blessed);
  Prometheus.Gauge.set packages_non_blessed (float_of_int non_blessed);
  Prometheus.Gauge.set packages_scanned (float_of_int scanned)
