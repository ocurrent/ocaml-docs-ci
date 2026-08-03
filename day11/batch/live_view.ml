type counts = { ok : int; fail : int; cascade : int }

type progress = {
  completed : int;
  total : int option;
  by_kind : (string * int) list;
  kind_totals : (string * int) list;
}

type t = {
  run_dir : Fpath.t;
  run_id : string;
  pkg_status : (string, string) Hashtbl.t;
  failed_dep : (string, string) Hashtbl.t;
  counts : counts;
  progress : progress;
}

let is_terminal ~snapshot_dir =
  Sys.file_exists (Fpath.to_string Fpath.(snapshot_dir / "status.json"))

let latest_run ~snapshot_dir =
  (* Pick the most recent run that has a [build.jsonl]. A run dir
     created in the same tick as [day11 status] runs might not yet
     have its first outcome — skipping them avoids "no run in
     progress" when there's a usable one one directory earlier. *)
  let runs_dir = Fpath.(snapshot_dir / "runs") in
  match Bos.OS.Dir.contents runs_dir with
  | Error _ -> None
  | Ok entries -> (
      entries
      |> List.filter (fun p ->
             Bos.OS.Dir.exists p |> Result.value ~default:false)
      |> List.filter (fun p ->
             Sys.file_exists (Fpath.to_string Fpath.(p / "build.jsonl")))
      |> List.sort (fun a b -> compare (Fpath.to_string b) (Fpath.to_string a))
      |> function
      | [] -> None
      | d :: _ -> Some d)

let read_jsonl path =
  let lines = ref [] in
  let ic = open_in (Fpath.to_string path) in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      try
        while true do
          let line = input_line ic in
          if String.length line > 0 then
            try lines := Yojson.Safe.from_string line :: !lines with _ -> ()
        done
      with End_of_file -> ());
  List.rev !lines

let read_json_opt path =
  try
    let data =
      In_channel.with_open_text (Fpath.to_string path) In_channel.input_all
    in
    Some (Yojson.Safe.from_string data)
  with _ -> None

let kind_totals_of_doc_dag json =
  let open Yojson.Safe.Util in
  let i name = match json |> member name with `Int n -> n | _ -> 0 in
  [
    ("build", i "build_nodes");
    ("tool", i "tool_nodes");
    ("compile", i "compile_nodes");
    ("doc-all", i "doc_all_nodes");
    ("link", i "link_nodes");
  ]

let load_latest ~snapshot_dir =
  match latest_run ~snapshot_dir with
  | None -> None
  | Some run_dir ->
      let jsonl = Fpath.(run_dir / "build.jsonl") in
      if not (Sys.file_exists (Fpath.to_string jsonl)) then None
      else
        let pkg_status : (string, string) Hashtbl.t = Hashtbl.create 256 in
        let failed_dep : (string, string) Hashtbl.t = Hashtbl.create 32 in
        let by_kind : (string, int) Hashtbl.t = Hashtbl.create 8 in
        let seen_hashes : (string, unit) Hashtbl.t = Hashtbl.create 256 in
        let counts = ref { ok = 0; fail = 0; cascade = 0 } in
        let completed = ref 0 in
        List.iter
          (fun json ->
            let open Yojson.Safe.Util in
            let pkg = json |> member "pkg" |> to_string in
            let status = json |> member "status" |> to_string in
            let kind =
              match json |> member "kind" with `String s -> s | _ -> "build"
            in
            let hash =
              match json |> member "hash" with `String s -> s | _ -> ""
            in
            (* Each hash counts once — lines can be rewritten on retry. *)
            if hash <> "" && not (Hashtbl.mem seen_hashes hash) then (
              Hashtbl.add seen_hashes hash ();
              incr completed;
              Hashtbl.replace by_kind kind
                ((try Hashtbl.find by_kind kind with Not_found -> 0) + 1);
              match status with
              | "ok" -> counts := { !counts with ok = !counts.ok + 1 }
              | "fail" -> counts := { !counts with fail = !counts.fail + 1 }
              | "cascade" ->
                  counts := { !counts with cascade = !counts.cascade + 1 }
              | _ -> ());
            (* pkg_status: an "ok" on any hash wins (a later version built). *)
            (match Hashtbl.find_opt pkg_status pkg with
            | Some "ok" -> ()
            | _ -> Hashtbl.replace pkg_status pkg status);
            match json |> member "failed_dep" with
            | `String dep -> Hashtbl.replace failed_dep pkg dep
            | _ -> ())
          (read_jsonl jsonl);
        let kind_totals =
          match read_json_opt Fpath.(run_dir / "doc_dag.json") with
          | Some j -> kind_totals_of_doc_dag j
          | None -> (
              match read_json_opt Fpath.(run_dir / "dag.json") with
              | Some j -> (
                  let open Yojson.Safe.Util in
                  match j |> member "build_nodes" with
                  | `Int n -> [ ("build", n) ]
                  | _ -> [])
              | None -> [])
        in
        let total =
          if kind_totals = [] then None
          else Some (List.fold_left (fun acc (_, n) -> acc + n) 0 kind_totals)
        in
        let progress =
          {
            completed = !completed;
            total;
            by_kind =
              Hashtbl.fold (fun k n acc -> (k, n) :: acc) by_kind []
              |> List.sort compare;
            kind_totals;
          }
        in
        Some
          {
            run_dir;
            run_id = Fpath.basename run_dir;
            pkg_status;
            failed_dep;
            counts = !counts;
            progress;
          }

let format_progress (p : progress) =
  let pct =
    match p.total with
    | Some t when t > 0 ->
        Printf.sprintf " (%.1f%%)" (100. *. float p.completed /. float t)
    | _ -> ""
  in
  let total_s =
    match p.total with Some t -> Printf.sprintf "/%d" t | None -> ""
  in
  let per_kind =
    let lookup kind = try List.assoc kind p.by_kind with Not_found -> 0 in
    let total_of kind =
      try List.assoc kind p.kind_totals with Not_found -> 0
    in
    let parts =
      List.filter_map
        (fun kind ->
          let done_ = lookup kind in
          let total = total_of kind in
          if done_ = 0 && total = 0 then None
          else if total > 0 then
            Some (Printf.sprintf "%s=%d/%d" kind done_ total)
          else Some (Printf.sprintf "%s=%d" kind done_))
        [ "build"; "tool"; "compile"; "doc-all"; "link" ]
    in
    if parts = [] then "" else " — " ^ String.concat " " parts
  in
  Printf.sprintf "%d%s%s%s" p.completed total_s pct per_kind
