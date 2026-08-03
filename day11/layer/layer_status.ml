type entry = { hash : string; exit_status : int; ts : string }

let path os_dir = Fpath.(os_dir / "layer_status.jsonl")

let now_iso8601 () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let entry_to_line e =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("hash", `String e.hash);
         ("exit_status", `Int e.exit_status);
         ("ts", `String e.ts);
       ])
  ^ "\n"

let entry_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let open Yojson.Safe.Util in
    Some
      {
        hash = json |> member "hash" |> to_string;
        exit_status = json |> member "exit_status" |> to_int;
        ts = json |> member "ts" |> to_string;
      }
  with _ -> None

(** Process-wide mutex serialising appends. The kernel's O_APPEND serialises
    across processes; the mutex serialises across our own workers so we issue
    one syscall per line cleanly. *)
let append_mutex = Mutex.create ()

(** Normalise to the 12-char prefix used as the layer dir name — that's the form
    already on disk and what callers query with. *)
let normalise_hash h = String.sub h 0 (min 12 (String.length h))

let append ~os_dir ~hash ~exit_status =
  let hash = normalise_hash hash in
  let line = entry_to_line { hash; exit_status; ts = now_iso8601 () } in
  let p = Fpath.to_string (path os_dir) in
  Mutex.lock append_mutex;
  let finally () = Mutex.unlock append_mutex in
  Fun.protect ~finally (fun () ->
      let fd =
        Unix.openfile p [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644
      in
      let bytes = Bytes.unsafe_of_string line in
      let len = Bytes.length bytes in
      let n = Unix.write fd bytes 0 len in
      Unix.close fd;
      if n <> len then
        Printf.eprintf "Layer_status.append: short write %d/%d for %s\n%!" n len
          hash)

(** Bootstrap: scan every [<os_dir>/<short>/layer.json], compose entries, write
    atomically via temp+rename so racing processes can't see a partial file.
    Returns the freshly-built table without re-reading the just-written file. *)
let bootstrap_from_disk os_dir =
  let table : (string, entry) Hashtbl.t = Hashtbl.create 32768 in
  let os_dir_s = Fpath.to_string os_dir in
  if not (Sys.file_exists os_dir_s) then table
  else
    let entries = Sys.readdir os_dir_s in
    Array.iter
      (fun name ->
        (* layer dirs are 12-char hex; skip everything else (locks,
         markers, the layer_status.jsonl file itself, etc.) *)
        if String.length name = 12 then
          let layer_json =
            Filename.concat (Filename.concat os_dir_s name) "layer.json"
          in
          match In_channel.with_open_text layer_json In_channel.input_all with
          | exception _ -> ()
          | s -> (
              try
                let json = Yojson.Safe.from_string s in
                let exit_status =
                  json
                  |> Yojson.Safe.Util.member "exit_status"
                  |> Yojson.Safe.Util.to_int
                in
                let e = { hash = name; exit_status; ts = now_iso8601 () } in
                Hashtbl.replace table name e
              with _ -> ()))
      entries;
    (* Atomically materialise the file. *)
    let final_path = Fpath.to_string (path os_dir) in
    let tmp_path = Printf.sprintf "%s.tmp.%d" final_path (Unix.getpid ()) in
    let oc = open_out tmp_path in
    Hashtbl.iter (fun _ e -> output_string oc (entry_to_line e)) table;
    close_out oc;
    (try Unix.rename tmp_path final_path
     with _ -> ( try Sys.remove tmp_path with _ -> ()));
    table

let load ~os_dir =
  let p = path os_dir in
  let p_s = Fpath.to_string p in
  if not (Sys.file_exists p_s) then bootstrap_from_disk os_dir
  else
    let table : (string, entry) Hashtbl.t = Hashtbl.create 32768 in
    let ic = open_in p_s in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        try
          while true do
            let line = input_line ic in
            if line <> "" then
              match entry_of_line line with
              | Some e ->
                  let key = normalise_hash e.hash in
                  Hashtbl.replace table key { e with hash = key }
              | None -> ()
          done
        with End_of_file -> ());
    table
