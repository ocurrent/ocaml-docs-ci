(* Eio-based parallel DAG execution with optional priority.

   Each node gets a promise in a hashtable. When first encountered, we
   await dependency promises then execute. Eio.Semaphore limits concurrency.
   When a priority function is given, ready nodes are dispatched in
   priority order (higher values run first).

   Cached nodes (identified by [is_cached]) have their promises
   pre-resolved so they never enter the executor loop. [on_complete] is
   still invoked for them, so callers see a single outcome stream for
   both fresh and cached builds. *)

open Eio.Std
module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool

type build = Build.t
type outcome = Ok | Failed | Cascaded
type cache_status = Not_cached | Cached_ok | Cached_fail

type stats = {
  total : int;
  completed : int;
  ok : int;
  failed : int;
  cascaded : int;
  cached : int;
}

let execute env ~np ~on_complete ~on_cascade ?(priority = fun _ -> 0)
    ?(is_cached = fun _ -> Not_cached) nodes build_one =
  let a_completed = Atomic.make 0 in
  let a_ok = Atomic.make 0 in
  let a_failed = Atomic.make 0 in
  let a_cascaded = Atomic.make 0 in
  let sem = Eio.Semaphore.make np in
  let promises : (string, outcome Promise.t) Hashtbl.t =
    Hashtbl.create (List.length nodes)
  in
  (* Pre-resolve cached nodes — no fibers needed. We still notify the
     caller via [on_complete] so the run log, recorder, and stats see
     cached outcomes in the same way they see fresh ones. *)
  let cached_outcomes : (build * bool) list ref = ref [] in
  List.iter
    (fun (node : build) ->
      match is_cached node with
      | Not_cached -> ()
      | Cached_ok ->
          let p, r = Promise.create () in
          Hashtbl.replace promises node.hash p;
          Promise.resolve r Ok;
          cached_outcomes := (node, true) :: !cached_outcomes
      | Cached_fail ->
          let p, r = Promise.create () in
          Hashtbl.replace promises node.hash p;
          Promise.resolve r Failed;
          cached_outcomes := (node, false) :: !cached_outcomes)
    nodes;
  let cached = List.length !cached_outcomes in
  let uncached =
    List.filter
      (fun (node : build) -> not (Hashtbl.mem promises node.hash))
      nodes
  in
  let total = List.length uncached in
  Printf.printf "  Executor: %d cached (pre-resolved), %d to run\n%!" cached
    total;
  let make_stats () =
    {
      total;
      completed = Atomic.get a_completed;
      ok = Atomic.get a_ok;
      failed = Atomic.get a_failed;
      cascaded = Atomic.get a_cascaded;
      cached;
    }
  in
  (* Notify the caller about cached outcomes before running uncached
     nodes, so callers see a deterministic order: cached first (in the
     order they were declared), then fresh builds as they complete.
     Cached outcomes don't count toward [completed]/[ok]/[failed] — those
     track fresh executor work — but they are reported via [on_complete]. *)
  List.iter
    (fun (node, success) ->
      on_complete ~stats:(make_stats ()) ~cached:true node success)
    (List.rev !cached_outcomes);
  let sorted_nodes =
    List.sort (fun a b -> compare (priority b) (priority a)) uncached
  in
  let rec run_node (node : build) : outcome =
    match Hashtbl.find_opt promises node.hash with
    | Some p -> Promise.await p
    | None ->
        let p, r = Promise.create () in
        Hashtbl.add promises node.hash p;
        let resolved, unresolved =
          List.partition
            (fun (dep : build) ->
              match Hashtbl.find_opt promises dep.hash with
              | Some dp -> Promise.is_resolved dp
              | None -> false)
            node.deps
        in
        let resolved_outcomes =
          List.map
            (fun (dep : build) ->
              Promise.await (Hashtbl.find promises dep.hash))
            resolved
        in
        let unresolved_outcomes =
          Fiber.List.map (fun dep -> run_node dep) unresolved
        in
        let dep_outcomes = resolved_outcomes @ unresolved_outcomes in
        let any_dep_failed =
          List.exists
            (fun o -> match o with Ok -> false | _ -> true)
            dep_outcomes
        in
        let outcome =
          if any_dep_failed then (
            let failed_dep =
              List.find
                (fun (dep : build) ->
                  match Hashtbl.find_opt promises dep.hash with
                  | Some p -> (
                      match Promise.await p with Ok -> false | _ -> true)
                  | None -> false)
                node.deps
            in
            ignore (Atomic.fetch_and_add a_completed 1);
            ignore (Atomic.fetch_and_add a_cascaded 1);
            on_cascade ~failed:node ~failed_dep;
            on_complete ~stats:(make_stats ()) ~cached:false node false;
            Cascaded)
          else (
            Eio.Semaphore.acquire sem;
            let success =
              Fun.protect
                ~finally:(fun () -> Eio.Semaphore.release sem)
                (fun () -> build_one node)
            in
            ignore (Atomic.fetch_and_add a_completed 1);
            if success then ignore (Atomic.fetch_and_add a_ok 1)
            else ignore (Atomic.fetch_and_add a_failed 1);
            on_complete ~stats:(make_stats ()) ~cached:false node success;
            if success then Ok else Failed)
        in
        Promise.resolve r outcome;
        outcome
  in
  ignore (env : Eio_unix.Stdenv.base);
  ignore (Fiber.List.map (fun node -> run_node node) sorted_nodes)
