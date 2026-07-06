(** The [/metrics] endpoint.

    Renders {!Prometheus.CollectorRegistry.default} — every metric defined
    in {!Docs_ci_lib.Metrics} plus OCurrent's built-in engine/cache
    metrics, the OCaml GC stats and per-log-level message counts that
    [prometheus-app] registers automatically — in the Prometheus text
    exposition format (version 0.0.4).

    We override [get_raw] rather than [get] because the latter wraps the
    body in the HTML dashboard template; a scrape target needs raw
    [text/plain]. The endpoint is unauthenticated, like a conventional
    Prometheus target — restrict it at the network / Caddy layer if the
    deployment needs it. *)

module Resource = Current_web.Resource

let r =
  object
    inherit Resource.t

    method! get_raw _site _request =
      let open Lwt.Infix in
      Prometheus.CollectorRegistry.(collect default) >>= fun snapshot ->
      let body =
        Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output snapshot
      in
      let headers =
        Cohttp.Header.init_with "Content-Type"
          "text/plain; version=0.0.4; charset=utf-8"
      in
      Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()
  end
