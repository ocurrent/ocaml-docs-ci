type entry = {
  ts : string;
  run : string;
  build_hash : string;
  status : string;
  category : string;
  blessed : bool;
  error : string option;
  universe : string;
      (** The node's output universe (doc nodes only; "" for build/tool nodes
          and for legacy entries written before this field existed). Persisted
          here at build time — known from the in-memory plan — so the package
          page can show it without parsing the large dag.json. *)
}

let entry_to_json (e : entry) : Yojson.Safe.t =
  let fields =
    [
      ("ts", `String e.ts);
      ("run", `String e.run);
      ("build_hash", `String e.build_hash);
      ("status", `String e.status);
      ("category", `String e.category);
      ("blessed", `Bool e.blessed);
    ]
  in
  let fields =
    match e.error with
    | Some v -> fields @ [ ("error", `String v) ]
    | None -> fields
  in
  let fields =
    if e.universe = "" then fields
    else fields @ [ ("universe", `String e.universe) ]
  in
  `Assoc fields

let string_of_json key assoc =
  match List.assoc_opt key assoc with Some (`String s) -> Some s | _ -> None

let bool_of_json key assoc =
  match List.assoc_opt key assoc with Some (`Bool b) -> Some b | _ -> None

let string_opt_of_json key assoc =
  match List.assoc_opt key assoc with
  | Some (`String s) -> Some (Some s)
  | Some `Null | None -> Some None
  | _ -> None

let entry_of_json (json : Yojson.Safe.t) : entry option =
  match json with
  | `Assoc assoc -> (
      (* [compiler], [failed_dep], [failed_dep_hash] may be present in
       legacy entries — read tolerantly and ignore them. *)
      match
        ( string_of_json "ts" assoc,
          string_of_json "run" assoc,
          string_of_json "build_hash" assoc,
          string_of_json "status" assoc,
          string_of_json "category" assoc,
          bool_of_json "blessed" assoc,
          string_opt_of_json "error" assoc )
      with
      | ( Some ts,
          Some run,
          Some build_hash,
          Some status,
          Some category,
          Some blessed,
          Some error ) ->
          let universe =
            match string_of_json "universe" assoc with
            | Some s -> s
            | None -> ""
          in
          Some
            { ts; run; build_hash; status; category; blessed; error; universe }
      | _ -> None)
  | _ -> None

let history_path ~packages_dir ~pkg_str =
  Fpath.to_string Fpath.(packages_dir / pkg_str / "history.jsonl")

let mkdir_p path =
  let rec create dir =
    if not (Sys.file_exists dir) then (
      create (Filename.dirname dir);
      try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  create path

(* Per-path Eio.Mutex table for intra-process serialisation.
   POSIX [lockf] alone is insufficient: it's keyed by (inode, pid),
   so two fibers in the same day11 process never block each other
   via the OS. The Eio.Mutex gates all in-process access to a given
   history file, and [lockf] still protects against a second day11
   process writing concurrently. *)
let path_mutexes : (string, Eio.Mutex.t) Hashtbl.t = Hashtbl.create 16
let path_mutexes_lock = Mutex.create ()

let mutex_for path =
  Mutex.lock path_mutexes_lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock path_mutexes_lock)
    (fun () ->
      match Hashtbl.find_opt path_mutexes path with
      | Some m -> m
      | None ->
          let m = Eio.Mutex.create () in
          Hashtbl.replace path_mutexes path m;
          m)

let append ~packages_dir ~pkg_str entry =
  let path = history_path ~packages_dir ~pkg_str in
  mkdir_p (Filename.dirname path);
  let line = Yojson.Safe.to_string (entry_to_json entry) in
  let mu = mutex_for path in
  Eio.Mutex.use_rw ~protect:true mu @@ fun () ->
  let fd =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644
  in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.lockf fd Unix.F_LOCK 0;
      let line = line ^ "\n" in
      let len = String.length line in
      let written = ref 0 in
      while !written < len do
        written :=
          !written + Unix.write_substring fd line !written (len - !written)
      done)

let read ~packages_dir ~pkg_str =
  let path = history_path ~packages_dir ~pkg_str in
  if not (Sys.file_exists path) then []
  else
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let entries = ref [] in
        (try
           while true do
             let line = input_line ic in
             if String.length line > 0 then
               let json = Yojson.Safe.from_string line in
               match entry_of_json json with
               | Some e -> entries := e :: !entries
               | None -> ()
           done
         with End_of_file -> ());
        !entries)

let read_latest ~packages_dir ~pkg_str =
  let entries = read ~packages_dir ~pkg_str in
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (e : entry) ->
      if Hashtbl.mem seen e.build_hash then false
      else (
        Hashtbl.add seen e.build_hash true;
        true))
    entries

let read_blessed ~packages_dir ~pkg_str =
  let entries = read ~packages_dir ~pkg_str in
  List.find_opt (fun (e : entry) -> e.blessed) entries

let parse_iso8601 s =
  try
    Scanf.sscanf s "%4d-%2d-%2dT%2d:%2d:%2d" (fun year month day hour min sec ->
        let tm =
          {
            Unix.tm_sec = sec;
            tm_min = min;
            tm_hour = hour;
            tm_mday = day;
            tm_mon = month - 1;
            tm_year = year - 1900;
            tm_wday = 0;
            tm_yday = 0;
            tm_isdst = false;
          }
        in
        let t, _ = Unix.mktime tm in
        t)
  with _ -> 0.0

let compact ~packages_dir ~pkg_str ~max_age_days =
  let path = history_path ~packages_dir ~pkg_str in
  if not (Sys.file_exists path) then ()
  else
    let entries = List.rev (read ~packages_dir ~pkg_str) in
    let now = Unix.gettimeofday () in
    let cutoff = now -. (float_of_int max_age_days *. 86400.0) in
    let rec process acc = function
      | [] -> List.rev acc
      | e :: rest -> (
          let is_old = parse_iso8601 e.ts < cutoff in
          if not is_old then process (e :: acc) rest
          else
            let rec collect_run run remaining =
              match remaining with
              | next :: tail
                when next.status = e.status
                     && next.build_hash = e.build_hash
                     && parse_iso8601 next.ts < cutoff ->
                  collect_run (next :: run) tail
              | _ -> (List.rev run, remaining)
            in
            let run, remaining = collect_run [ e ] rest in
            match run with
            | [ single ] -> process (single :: acc) remaining
            | first :: _ ->
                let last = List.nth run (List.length run - 1) in
                if first == last then process (first :: acc) remaining
                else process (last :: first :: acc) remaining
            | [] -> process acc remaining)
    in
    let compacted = process [] entries in
    let tmp_path = path ^ ".tmp" in
    let oc = open_out tmp_path in
    Fun.protect
      ~finally:(fun () -> close_out oc)
      (fun () ->
        List.iter
          (fun e ->
            output_string oc (Yojson.Safe.to_string (entry_to_json e));
            output_char oc '\n')
          compacted);
    Sys.rename tmp_path path
