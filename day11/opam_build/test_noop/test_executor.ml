(* No-op executor test: load the DAG, run the executor with a build
   function that just sleeps briefly, measure parallelism. *)

let () =
  let dag_file = Sys.argv.(1) in
  let np = int_of_string Sys.argv.(2) in
  let sleep_ms = float_of_string Sys.argv.(3) in
  (* Load DAG from JSONL *)
  let nodes_by_hash : (string, Day11_opam_layer.Build.t) Hashtbl.t =
    Hashtbl.create 100000
  in
  let ic = open_in dag_file in
  let count = ref 0 in
  (try
     while true do
       let line = input_line ic in
       let json = Yojson.Safe.from_string line in
       let open Yojson.Safe.Util in
       let hash = json |> member "hash" |> to_string in
       let pkg_str = json |> member "pkg" |> to_string in
       let dep_hashes =
         json |> member "deps" |> to_list |> List.map to_string
       in
       let pkg = OpamPackage.of_string pkg_str in
       (* Store without deps first, patch later *)
       let node : Day11_opam_layer.Build.t =
         { hash; pkg; deps = []; universe = Day11_solution.Universe.dummy }
       in
       Hashtbl.replace nodes_by_hash hash node;
       ignore dep_hashes;
       incr count
     done
   with End_of_file -> close_in ic);
  Printf.printf "Loaded %d nodes\n%!" !count;
  (* Second pass: resolve deps *)
  let ic = open_in dag_file in
  (try
     while true do
       let line = input_line ic in
       let json = Yojson.Safe.from_string line in
       let open Yojson.Safe.Util in
       let hash = json |> member "hash" |> to_string in
       let dep_hashes =
         json |> member "deps" |> to_list |> List.map to_string
       in
       let deps =
         List.filter_map
           (fun dh -> Hashtbl.find_opt nodes_by_hash dh)
           dep_hashes
       in
       let pkg = (Hashtbl.find nodes_by_hash hash).pkg in
       Hashtbl.replace nodes_by_hash hash
         {
           Day11_opam_layer.Build.hash;
           pkg;
           deps;
           universe = Day11_solution.Universe.dummy;
         }
     done
   with End_of_file -> close_in ic);
  let all_nodes = Hashtbl.fold (fun _ n acc -> n :: acc) nodes_by_hash [] in
  let all_nodes =
    List.sort
      (fun (a : Day11_opam_layer.Build.t) b ->
        compare (List.length a.deps) (List.length b.deps))
      all_nodes
  in
  Printf.printf "Running executor with %d workers, %.0fms sleep...\n%!" np
    sleep_ms;
  let active = Atomic.make 0 in
  let max_active = Atomic.make 0 in
  let t0 = Unix.gettimeofday () in
  Eio_main.run @@ fun _env ->
  Day11_opam_build.Dag_executor.execute
    (_env :> Eio_unix.Stdenv.base)
    ~np
    ~on_complete:(fun ~stats ~cached:_ _node _success ->
      if stats.completed mod 1000 = 0 then
        let elapsed = Unix.gettimeofday () -. t0 in
        Printf.printf "  [%d/%d] %.1fs max_active=%d\n%!" stats.completed
          stats.total elapsed (Atomic.get max_active))
    ~on_cascade:(fun ~failed:_ ~failed_dep:_ -> ())
    all_nodes
    (fun _node ->
      let n = Atomic.fetch_and_add active 1 + 1 in
      let cur_max = Atomic.get max_active in
      if n > cur_max then Atomic.set max_active n;
      if sleep_ms > 0. then
        Eio.Time.sleep (Eio.Stdenv.clock _env) (sleep_ms /. 1000.);
      ignore (Atomic.fetch_and_add active (-1));
      true);
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf "Done in %.1fs, max_active=%d\n%!" elapsed
    (Atomic.get max_active)
