type t = OpamPackage.Set.t OpamPackage.Map.t

(* Transitive closure of a dependency graph that may contain cycles.

   A naive memoised DFS is WRONG here: on a back-edge it returns the
   empty set and then memoises the resulting truncated closure, so a
   node's computed closure depends on the DFS start order (which differs
   between solutions) and is incomplete — the cause of the doc-universe
   fragmentation. The correct, order-independent method is the standard
   one: find the strongly-connected components (Tarjan), collapse each to
   a super-node so the condensation is a DAG, and compute the closure
   over that DAG. Every node in an SCC shares the SCC's closure (they are
   mutually reachable).

   [closure(pkg)] is the set of packages reachable from [pkg] via one or
   more edges. A node is therefore in its own closure iff it lies on a
   cycle (non-trivial SCC or self-loop); an acyclic node is not — which
   matches the previous function's behaviour for acyclic graphs, so
   layer/universe hashes for acyclic packages are unchanged. *)
let transitive_deps deps =
  let direct pkg =
    match OpamPackage.Map.find_opt pkg deps with
    | Some s -> s
    | None -> OpamPackage.Set.empty
  in
  (* Tarjan's SCC. [index]/[low]/[on_stack]/[stack] are the usual state;
     [scc_of] maps each node to its component id, [scc_members] the
     reverse, and [completed] lists component ids in completion order —
     which is reverse-topological, i.e. a component's successors are
     completed before it. *)
  let index = Hashtbl.create 256 in
  let low = Hashtbl.create 256 in
  let on_stack = Hashtbl.create 256 in
  let stack = ref [] in
  let counter = ref 0 in
  let scc_of = Hashtbl.create 256 in
  let scc_members = Hashtbl.create 256 in
  let completed = ref [] in
  let nscc = ref 0 in
  let rec connect v =
    Hashtbl.replace index v !counter;
    Hashtbl.replace low v !counter;
    incr counter;
    stack := v :: !stack;
    Hashtbl.replace on_stack v true;
    OpamPackage.Set.iter
      (fun w ->
        if not (Hashtbl.mem index w) then (
          connect w;
          Hashtbl.replace low v (min (Hashtbl.find low v) (Hashtbl.find low w)))
        else if try Hashtbl.find on_stack w with Not_found -> false then
          Hashtbl.replace low v
            (min (Hashtbl.find low v) (Hashtbl.find index w)))
      (direct v);
    if Hashtbl.find low v = Hashtbl.find index v then (
      let id = !nscc in
      incr nscc;
      let rec pop acc =
        match !stack with
        | w :: rest ->
            stack := rest;
            Hashtbl.replace on_stack w false;
            Hashtbl.replace scc_of w id;
            let acc = w :: acc in
            if OpamPackage.equal w v then acc else pop acc
        | [] -> acc
      in
      let members = pop [] in
      Hashtbl.replace scc_members id members;
      completed := id :: !completed)
  in
  OpamPackage.Map.iter
    (fun v _ -> if not (Hashtbl.mem index v) then connect v)
    deps;
  (* Closure per component, in completion order (successors first), so
     each successor's closure is ready. [completed] was built by
     prepending, so its head is the last-completed (a source); iterate
     its reverse to get successors-first. *)
  let scc_closure = Hashtbl.create 256 in
  List.iter
    (fun id ->
      let members = Hashtbl.find scc_members id in
      let cyclic =
        match members with
        | _ :: _ :: _ -> true (* non-trivial SCC *)
        | [ m ] -> OpamPackage.Set.mem m (direct m) (* self-loop *)
        | [] -> false
      in
      let acc =
        ref
          (if cyclic then OpamPackage.Set.of_list members
           else OpamPackage.Set.empty)
      in
      List.iter
        (fun u ->
          OpamPackage.Set.iter
            (fun w ->
              if Hashtbl.find scc_of w <> id then (* inter-component edge *)
                acc :=
                  OpamPackage.Set.add w
                    (OpamPackage.Set.union !acc
                       (Hashtbl.find scc_closure (Hashtbl.find scc_of w))))
            (direct u))
        members;
      Hashtbl.replace scc_closure id !acc)
    (List.rev !completed);
  OpamPackage.Map.mapi
    (fun pkg _ -> Hashtbl.find scc_closure (Hashtbl.find scc_of pkg))
    deps

(* DFS with a three-colour marker: white (not seen), grey (on stack),
   black (done). A grey re-entry means we found a back-edge — i.e. a
   cycle. Stops at the first cycle. *)
let has_cycle deps =
  let colour : (OpamPackage.t, [ `Grey | `Black ]) Hashtbl.t =
    Hashtbl.create 64
  in
  let exception Cycle in
  let rec visit pkg =
    match Hashtbl.find_opt colour pkg with
    | Some `Black -> ()
    | Some `Grey -> raise Cycle
    | None ->
        Hashtbl.add colour pkg `Grey;
        (match OpamPackage.Map.find_opt pkg deps with
        | Some direct -> OpamPackage.Set.iter visit direct
        | None -> ());
        Hashtbl.replace colour pkg `Black
  in
  try
    OpamPackage.Map.iter (fun pkg _ -> visit pkg) deps;
    false
  with Cycle -> true

let compiler_names = [ "ocaml-base-compiler"; "ocaml-variants"; "ocaml" ]

let extract_ocaml_version deps =
  List.find_map
    (fun name ->
      let name = OpamPackage.Name.of_string name in
      OpamPackage.Map.filter
        (fun pkg _ -> OpamPackage.Name.equal (OpamPackage.name pkg) name)
        deps
      |> OpamPackage.Map.min_binding_opt
      |> Option.map fst)
    compiler_names
