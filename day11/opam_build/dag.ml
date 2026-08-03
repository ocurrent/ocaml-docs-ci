module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool

type build = Build.t

(* See {!Day11_doc.Generate}'s cooperative_yield — same rationale:
   this walk digests every solution's closures on the daemon's single
   domain and would otherwise starve the event loop for seconds. *)
let cooperative_yield =
  let n = ref 0 in
  fun () ->
    incr n;
    if !n land 63 = 0 then try Eio.Fiber.yield () with _ -> ()

let build_dag cache ~base_hash solutions =
  let t0 = Unix.gettimeofday () in
  (* Memo by build hash. The hash is the only true identity of a
     build: two solutions can reach [pkg] with the same build-deps
     closure but different doc-deps closures (different universes) —
     they produce identical layer content, and the downstream
     [build_by_hash] / cache lookup already collapses them. Memoing
     by [(pkg, universe)] used to produce one [Build.t] record per
     universe and dump them all into the dag.json list, leaving the
     serialised file with up to 57× identical entries per node
     (csexp.1.5.2 tool, dune-configurator.3.23.1 tool, etc.). Keyed
     by hash, the memo carries one node per actual build, and the
     [universe] field stored on the node is just the first-touching
     solution's view — that's already the case post-dedup downstream,
     so no information is lost. *)
  let memo : (string, build) Hashtbl.t = Hashtbl.create 256 in
  (* [local] memoises (pkg → node) within one solution. Without it,
     the [layer_hash] below — an O(closure) string build + digest even
     on its own cache-hit path — runs once per DAG {e edge} (every
     recursive [get_node] call from a depender), which dominated
     wall-clock on large profiles. Within a solution a package's node
     is fixed, so per-(solution, pkg) is the right granularity; the
     global [memo] by hash still dedups across solutions. *)
  let rec get_node local solution trans_build trans_doc pkg =
    let pkg_key = OpamPackage.to_string pkg in
    match Hashtbl.find_opt local pkg_key with
    | Some node -> node
    | None ->
        let node = get_node_uncached local solution trans_build trans_doc pkg in
        Hashtbl.replace local pkg_key node;
        node
  and get_node_uncached local solution trans_build trans_doc pkg =
    let pkg_build_deps =
      match OpamPackage.Map.find_opt pkg trans_build with
      | Some s -> OpamPackage.Set.elements s
      | None -> []
    in
    let all_pkgs = pkg :: pkg_build_deps in
    let hash = Hash_cache.layer_hash cache ~base_hash all_pkgs in
    match Hashtbl.find_opt memo hash with
    | Some node -> node
    | None ->
        (* Universe identity reflects the {b doc-deps} closure. Two
         solutions sharing build-deps but differing in doc-deps will
         hash-collide here (same [hash]); the first-arriving one's
         universe is the one we keep — same convention as
         [build_by_hash]'s last-write-wins. Falls back to build-deps
         when [pkg] isn't in [trans_doc] (defensive — shouldn't
         happen, since doc_solution ⊇ solution). *)
        let pkg_universe_deps =
          match OpamPackage.Map.find_opt pkg trans_doc with
          | Some s -> OpamPackage.Set.elements s
          | None -> pkg_build_deps
        in
        let universe =
          Day11_solution.Universe.of_deps
            (OpamPackage.Set.of_list pkg_universe_deps)
        in
        let direct_deps =
          match OpamPackage.Map.find_opt pkg solution with
          | Some s -> OpamPackage.Set.elements s
          | None -> []
        in
        let deps =
          List.filter_map
            (fun dep ->
              if OpamPackage.Map.mem dep solution then
                Some (get_node local solution trans_build trans_doc dep)
              else None)
            direct_deps
        in
        let node : build = { hash; pkg; deps; universe } in
        Hashtbl.replace memo hash node;
        node
  in
  List.iter
    (fun (_target, solution, doc_solution) ->
      let trans_build = Day11_solution.Deps.transitive_deps solution in
      let trans_doc = Day11_solution.Deps.transitive_deps doc_solution in
      let local : (string, build) Hashtbl.t =
        Hashtbl.create (OpamPackage.Map.cardinal solution)
      in
      OpamPackage.Map.iter
        (fun pkg _deps ->
          cooperative_yield ();
          ignore (get_node local solution trans_build trans_doc pkg))
        solution)
    solutions;
  let all_nodes = Hashtbl.fold (fun _ node acc -> node :: acc) memo [] in
  let sorted =
    List.sort
      (fun (a : build) (b : build) ->
        compare (List.length a.deps) (List.length b.deps))
      all_nodes
  in
  Printf.printf "  build_dag: %d nodes from %d solutions in %.1fs\n%!"
    (List.length sorted) (List.length solutions)
    (Unix.gettimeofday () -. t0);
  sorted
