(* ── Phase timing ─────────────────────────────────────────────── *)

type timing = (string * float) list

let empty_timing : timing = []
let timing_field name t = try List.assoc name t with Not_found -> 0.0

let timing_to_yojson (t : timing) : Yojson.Safe.t =
  `Assoc (List.map (fun (k, v) -> (k, `Float v)) t)

let timing_of_yojson (json : Yojson.Safe.t) : (timing, string) result =
  match json with
  | `Assoc kvs -> (
      try
        Ok
          (List.map
             (fun (k, v) ->
               match v with
               | `Float f -> (k, f)
               | `Int i -> (k, float_of_int i)
               | _ -> failwith "timing value must be a number")
             kvs)
      with Failure m -> Error m)
  | `Null -> Ok []
  | _ -> Error "timing must be a JSON object"

(* ── Layer metadata ────────────────────────────────────────────── *)

type t = {
  exit_status : int;
  parent_hashes : string list; [@default []]
  uid : int;
  gid : int;
  base_hash : string;
  disk_usage : int; [@default 0]
  timing : timing; [@default empty_timing]
  created_at : string;
  failed_dep : string option; [@default None]
}
[@@deriving yojson { strict = false }]

(* ── (de)serialization ────────────────────────────────────────── *)

let now_iso8601 env =
  let t = Eio.Time.now env#clock in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.tm_year + 1900)
    (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let save ?created_at env path t =
  let created_at =
    match created_at with
    | Some s -> s
    | None -> if t.created_at = "" then now_iso8601 env else t.created_at
  in
  let t = { t with created_at } in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) (eio_path env path)
      (Yojson.Safe.to_string (to_yojson t));
    Ok ()
  with exn ->
    Rresult.R.error_msgf "save %a: %s" Fpath.pp path (Printexc.to_string exn)

let load env path =
  match
    try Ok (Eio.Path.load (eio_path env path))
    with exn ->
      Rresult.R.error_msgf "load %a: %s" Fpath.pp path (Printexc.to_string exn)
  with
  | Error _ as e -> e
  | Ok s -> (
      match
        try Ok (Yojson.Safe.from_string s)
        with exn ->
          Rresult.R.error_msgf "parse %a: %s" Fpath.pp path
            (Printexc.to_string exn)
      with
      | Error _ as e -> e
      | Ok json -> (
          match of_yojson json with
          | Ok v -> Ok v
          | Error msg -> Rresult.R.error_msgf "load %a: %s" Fpath.pp path msg))
