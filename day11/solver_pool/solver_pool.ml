let src =
  Logs.Src.create "day11.solver_pool" ~doc:"Parallel out-of-process solver"

module Log = (val Logs.src_log src)

(* Bumped by every [solve_many] call so concurrent invocations get
   distinct output filenames. *)
let solve_many_call_seq = Atomic.make 0

let find_worker_bin () =
  let exe_dir = Filename.dirname Sys.argv.(0) in
  let candidates =
    [
      Filename.concat exe_dir "solver_worker.exe";
      Filename.concat exe_dir "../solver/solver_worker.exe";
      Filename.concat exe_dir "../../day11/solver/solver_worker.exe";
      "_build/default/day11/solver/solver_worker.exe";
      "day11-solver-worker";
    ]
  in
  let from_path =
    (* Check $PATH for day11-solver-worker. This only runs once per
       solve_many call, before Eio fibers are spawned, so using plain
       Unix I/O is fine. *)
    let ic = Unix.open_process_in "which day11-solver-worker 2>/dev/null" in
    let path = try Some (String.trim (input_line ic)) with _ -> None in
    ignore (Unix.close_process_in ic);
    path
  in
  let all_candidates =
    candidates @ match from_path with Some p -> [ p ] | None -> []
  in
  match List.find_opt Sys.file_exists all_candidates with
  | Some p -> p
  | None ->
      let tried = String.concat ", " candidates in
      failwith
        (Printf.sprintf "solver_worker binary not found (tried: %s, argv0=%s)"
           tried Sys.argv.(0))

let examined_of_json json =
  let open Yojson.Safe.Util in
  json
  |> to_list
  |> List.map to_string
  |> List.map OpamPackage.Name.of_string
  |> OpamPackage.Name.Set.of_list

let parse_result_line line =
  let json = Yojson.Safe.from_string line in
  let open Yojson.Safe.Util in
  let pkg = json |> member "package" |> to_string |> OpamPackage.of_string in
  match json |> member "failed" |> to_bool_option with
  | Some true ->
      let error = json |> member "error" |> to_string in
      let examined = json |> member "examined" |> examined_of_json in
      (pkg, Error (error, examined))
  | _ -> (
      match Day11_solution.Solve_result.of_json (json |> member "result") with
      | Ok result -> (pkg, Ok result)
      | Error (`Msg e) -> (pkg, Error (e, OpamPackage.Name.Set.empty)))

let parse_output_file path =
  (* One-shot read of the JSONL output file after the worker exits.
     The file is small (bounded by batch size) and only read once, so
     using Eio.Path.load is simpler than streaming. *)
  let contents = Eio.Path.load path in
  let results = ref [] in
  String.split_on_char '\n' contents
  |> List.iter (fun line ->
      if line <> "" then
        match parse_result_line line with
        | r -> results := r :: !results
        | exception _ -> ());
  List.rev !results

let solve_many ~sw env ?(pin_dirs = []) ?(constraints = [])
    ?(extra_targets = []) ?(doc = true) ?(pin_target = true) ?ocaml_version
    ?on_progress ~np ~repos targets =
  if targets = [] then []
  else
    let np = max 1 (min np (List.length targets)) in
    let worker_bin = find_worker_bin () in
    Log.debug (fun m ->
        m "Solving %d targets with %d workers" (List.length targets) np);
    (* Partition targets across [np] workers. *)
    let batches = Array.make np [] in
    List.iteri
      (fun i target ->
        let slot = i mod np in
        batches.(slot) <- target :: batches.(slot))
      targets;
    let tmp_dir = Filename.get_temp_dir_name () in
    let pid = Unix.getpid () in
    (* Per-call unique tag so concurrent [solve_many] invocations don't
     write to the same output file. The previous [{pid}_{slot}] scheme
     collided when multiple fibers each invoked [solve_many ~np:1]
     (same pid, same slot=0). *)
    let call_tag =
      Printf.sprintf "%x_%d" (Random.bits ())
        (Atomic.fetch_and_add solve_many_call_seq 1)
    in
    let total = List.length targets in
    let done_count = Atomic.make 0 in
    let fs = Eio.Stdenv.fs env in
    (* Run each batch in a fiber. Use Fiber.List.map with max_fibers:np
     so all workers run concurrently (bounded by np). [sw] lets
     cancellation propagate to spawned workers via Sys.Run. *)
    let batch_idxs = List.init np (fun i -> i) in
    let batch_results =
      Eio.Fiber.List.map ~max_fibers:np
        (fun slot ->
          let batch = List.rev batches.(slot) in
          if batch = [] then []
          else
            let output_file =
              Filename.concat tmp_dir
                (Printf.sprintf "day11_solve_%d_%s_%d.jsonl" pid call_tag slot)
            in
            let repo_args =
              List.concat_map
                (fun (repo, sha) -> [ "--repo"; repo ^ ":" ^ sha ])
                repos
            in
            let ocaml_args =
              match ocaml_version with
              | Some pkg -> [ "--ocaml-version"; OpamPackage.to_string pkg ]
              | None -> []
            in
            let pin_args =
              List.concat_map (fun dir -> [ "--pin-dir"; dir ]) pin_dirs
            in
            let constraint_args =
              List.concat_map
                (fun pkg -> [ "--constraint"; OpamPackage.to_string pkg ])
                constraints
            in
            let extra_target_args =
              List.concat_map
                (fun pkg -> [ "--extra-target"; OpamPackage.to_string pkg ])
                extra_targets
            in
            let doc_args = if doc then [] else [ "--no-doc" ] in
            let pin_target_args =
              if pin_target then [] else [ "--no-pin-target" ]
            in
            let cmd =
              Bos.Cmd.(
                v worker_bin
                %% of_list repo_args
                % "--output"
                % output_file
                %% of_list ocaml_args
                %% of_list pin_args
                %% of_list constraint_args
                %% of_list extra_target_args
                %% of_list doc_args
                %% of_list pin_target_args
                %% of_list (List.map OpamPackage.to_string batch))
            in
            let out_fpath = Fpath.v output_file in
            let _run : Day11_sys.Run.t =
              Day11_sys.Run.run ~sw env cmd (Some out_fpath)
            in
            let results =
              try parse_output_file Eio.Path.(fs / output_file) with _ -> []
            in
            (* Remove the temp file. Ignore errors — the worst case is a
           dangling file in TMPDIR, cleaned up by the OS. *)
            (try Eio.Path.unlink Eio.Path.(fs / output_file) with _ -> ());
            let delta = List.length results in
            let new_done = Atomic.fetch_and_add done_count delta + delta in
            (match on_progress with
            | Some f -> f ~done_count:new_done ~total
            | None -> ());
            results)
        batch_idxs
    in
    List.concat batch_results
