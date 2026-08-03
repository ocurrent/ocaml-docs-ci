(** Eio-based parallel DAG execution.

    Executes build nodes in dependency order using Eio fibers and promise-based
    memoization. Each node becomes a fiber that awaits its dependency promises
    before building.

    Cached nodes (identified by [is_cached]) have their promises pre-resolved so
    they never enter the executor loop, but [on_complete] is still invoked for
    them with [~cached:true] so callers see a single outcome stream. *)

type cache_status = Not_cached | Cached_ok | Cached_fail

type stats = {
  total : int;
  completed : int;
  ok : int;
  failed : int;
  cascaded : int;
  cached : int;
}

val execute :
  Eio_unix.Stdenv.base ->
  np:int ->
  on_complete:
    (stats:stats -> cached:bool -> Day11_opam_layer.Build.t -> bool -> unit) ->
  on_cascade:
    (failed:Day11_opam_layer.Build.t ->
    failed_dep:Day11_opam_layer.Build.t ->
    unit) ->
  ?priority:(Day11_opam_layer.Build.t -> int) ->
  ?is_cached:(Day11_opam_layer.Build.t -> cache_status) ->
  Day11_opam_layer.Build.t list ->
  (Day11_opam_layer.Build.t -> bool) ->
  unit
(** [execute env ~np ~on_complete ~on_cascade ?priority ?is_cached nodes
     build_one] executes [nodes] in dependency order with up to [np] concurrent
    workers. Nodes where [is_cached] returns [Cached_ok] or [Cached_fail] are
    pre-resolved and [build_one] is not called for them, but [on_complete] is
    invoked with [~cached:true] so callers can log or record the outcome.
    [Cached_fail] nodes propagate failure to dependents (triggering cascades).
*)
