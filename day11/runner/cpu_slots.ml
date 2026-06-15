let src = Logs.Src.create "day11.runner.cpu_slots"
  ~doc:"NUMA-aware cpu slot pool"
module Log = (val Logs.src_log src)

type slot = {
  cpuset : string;
  numa_mems : string option;
  node : int;
}

type t = {
  slots : slot Eio.Stream.t;
  n_slots : int;
  cores_per_build : int;
  layout : string;
}

(* Expand a Linux cpuset list ("0-3,6,10-11") into a sorted int list. *)
let expand_cpulist s =
  let parse_range r =
    match String.split_on_char '-' r with
    | [ a ] -> [ int_of_string (String.trim a) ]
    | [ a; b ] ->
      let a = int_of_string (String.trim a) in
      let b = int_of_string (String.trim b) in
      List.init (b - a + 1) (fun i -> a + i)
    | _ -> []
  in
  String.split_on_char ',' s
  |> List.concat_map (fun r ->
    let r = String.trim r in
    if r = "" then [] else parse_range r)
  |> List.sort_uniq compare

(* Render an int list back into compact Linux cpuset notation,
   collapsing runs of consecutive CPUs into "a-b". *)
let format_cpuset cpus =
  let sorted = List.sort_uniq compare cpus in
  let rec go runs start prev = function
    | [] -> List.rev ((start, prev) :: runs)
    | x :: xs when x = prev + 1 -> go runs start x xs
    | x :: xs -> go ((start, prev) :: runs) x x xs
  in
  match sorted with
  | [] -> ""
  | x :: xs ->
    go [] x x xs
    |> List.map (fun (a, b) ->
      if a = b then string_of_int a
      else Printf.sprintf "%d-%d" a b)
    |> String.concat ","

let read_file path =
  try Some (In_channel.with_open_text path In_channel.input_all)
  with _ -> None

(* Read [/sys/devices/system/node/node*/cpulist]. Returns the list of
   [(node, cpus)] pairs sorted by node index, or [None] if NUMA
   layout isn't exposed (e.g. on non-Linux or containers that mask
   [/sys]). *)
let detect_numa () =
  let base = "/sys/devices/system/node" in
  if not (Sys.file_exists base && Sys.is_directory base) then None
  else
    let entries =
      try Sys.readdir base |> Array.to_list
      with _ -> [] in
    let nodes =
      List.filter_map (fun name ->
        if String.length name > 4 && String.sub name 0 4 = "node" then
          try
            Some (int_of_string
                    (String.sub name 4 (String.length name - 4)))
          with _ -> None
        else None) entries
      |> List.sort compare in
    if nodes = [] then None
    else
      let pairs = List.filter_map (fun n ->
        let path = Printf.sprintf "%s/node%d/cpulist" base n in
        match read_file path with
        | Some s ->
          let cpus = expand_cpulist (String.trim s) in
          if cpus = [] then None else Some (n, cpus)
        | None -> None) nodes
      in
      if pairs = [] then None else Some pairs

(* Fallback when NUMA info isn't available: single pool of all online
   CPUs. Uses [/sys/devices/system/cpu/online] if present, falls back
   to [nproc]. *)
let detect_flat () =
  match read_file "/sys/devices/system/cpu/online" with
  | Some s ->
    let cpus = expand_cpulist (String.trim s) in
    if cpus = [] then
      List.init (max 1 (Domain.recommended_domain_count ()))
        (fun i -> i)
    else cpus
  | None ->
    let n =
      try Domain.recommended_domain_count ()
      with _ -> 1 in
    List.init (max 1 n) (fun i -> i)

(* Split [cpus] into disjoint chunks of size [n]. Any tail shorter
   than [n] is dropped — we'd rather leave a few host CPUs idle than
   have one oversize/undersize slot. *)
let chunk_into n cpus =
  let rec go acc cur count = function
    | [] ->
      if count = n then List.rev (List.rev cur :: acc)
      else List.rev acc
    | x :: xs when count = n ->
      go (List.rev cur :: acc) [ x ] 1 xs
    | x :: xs -> go acc (x :: cur) (count + 1) xs
  in
  go [] [] 0 cpus

(* Interleave base slots by NUMA node so consecutive indices alternate
   across nodes. This keeps overcommit duplication balanced: for
   overcommit=1.5, the 0.5 "extra" slots get spread across nodes
   instead of piling onto whichever node happens to come first in
   [detect_numa]'s listing. *)
let interleave_by_node base =
  let buckets : (int, slot list ref) Hashtbl.t = Hashtbl.create 4 in
  List.iter (fun s ->
    let r = try Hashtbl.find buckets s.node
      with Not_found ->
        let r = ref [] in Hashtbl.add buckets s.node r; r in
    r := s :: !r) base;
  let queues = Hashtbl.fold (fun k v acc -> (k, v) :: acc) buckets []
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map (fun (_, r) -> ref (List.rev !r)) in
  let rec drain acc =
    let any_left = ref false in
    let acc = List.fold_left (fun acc q ->
      match !q with
      | [] -> acc
      | s :: rest -> any_left := true; q := rest; s :: acc
    ) acc queues in
    if !any_left then drain acc else acc
  in
  List.rev (drain [])

(* Replicate each slot enough times so the total count is
   [round(n_base * overcommit)]. [base] should already be
   interleaved by NUMA node so [i mod n_base] spreads duplicates
   evenly — otherwise fractional overcommit can pile onto one node. *)
let apply_overcommit ~overcommit base =
  let overcommit = Float.max 1.0 overcommit in
  let n_base = List.length base in
  let n_total =
    Float.to_int (Float.round (Float.of_int n_base *. overcommit)) in
  let n_total = max n_base n_total in
  let arr = Array.of_list base in
  List.init n_total (fun i -> arr.(i mod n_base))

let auto ?(overcommit = 1.0) ~cores_per_build () =
  if cores_per_build < 1 then
    invalid_arg "Cpu_slots.auto: cores_per_build must be >= 1";
  let base_slots, layout_desc =
    match detect_numa () with
    | Some pairs when List.length pairs >= 2 ->
      (* NUMA-aware path: pin each slot to one node's CPUs. Memory is
         deliberately NOT pinned (no cpuset.mems): first-touch from the
         pinned CPUs already lands a build's pages on the local node,
         while a hard mems binding turns one node filling up into a
         cpuset-constrained OOM kill — observed taking out the whole
         batch driver while the host had terabytes free on other
         nodes. *)
      let slots = List.concat_map (fun (node, cpus) ->
        chunk_into cores_per_build cpus
        |> List.map (fun chunk ->
          { cpuset = format_cpuset chunk;
            numa_mems = None;
            node })
      ) pairs in
      let desc = pairs
        |> List.map (fun (n, cpus) ->
          Printf.sprintf "node%d:%d" n (List.length cpus))
        |> String.concat " " in
      slots, "numa(" ^ desc ^ ")"
    | _ ->
      (* Flat path: no NUMA pinning, all slots on one big pool. *)
      let cpus = detect_flat () in
      let slots = chunk_into cores_per_build cpus
        |> List.map (fun chunk ->
          { cpuset = format_cpuset chunk;
            numa_mems = None;
            node = 0 }) in
      slots, Printf.sprintf "flat(%d cpus)" (List.length cpus)
  in
  if base_slots = [] then
    invalid_arg (Printf.sprintf
      "Cpu_slots.auto: no slots produced (cores_per_build=%d, layout=%s) — \
       host has fewer CPUs than one slot needs"
      cores_per_build layout_desc);
  let base_slots = interleave_by_node base_slots in
  let slots = apply_overcommit ~overcommit base_slots in
  let n_base = List.length base_slots in
  let n = List.length slots in
  let stream = Eio.Stream.create n in
  List.iter (fun s -> Eio.Stream.add stream s) slots;
  Log.info (fun m -> m "CPU slot pool: %d slots × %d cpus (%s, overcommit=%.2fx of %d base)"
    n cores_per_build layout_desc overcommit n_base);
  { slots = stream; n_slots = n; cores_per_build; layout = layout_desc }

let n_slots t = t.n_slots
let cores_per_build t = t.cores_per_build
let describe t =
  Printf.sprintf "%d slots × %d cpus (%s)"
    t.n_slots t.cores_per_build t.layout

let acquire t = Eio.Stream.take t.slots
let release t slot = Eio.Stream.add t.slots slot

let with_slot t f =
  let slot = acquire t in
  Fun.protect ~finally:(fun () -> release t slot)
    (fun () -> f slot)
