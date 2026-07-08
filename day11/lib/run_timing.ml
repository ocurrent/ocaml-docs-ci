(* Wall-clock timing for a snapshot's first build-to-completion.

   A snapshot dir is keyed by its opam-repository set (Snapshot.compute_key),
   so a fresh opam-repo / overlay commit yields a new dir — and hence a fresh
   pair of timestamps. Both markers are write-once: [started_at] the first
   time the snapshot enters the pipeline, [completed_at] the first time it
   reaches completion. Persisting them (rather than timing in memory) keeps
   the duration stable across the daemon's frequent pipeline re-evaluations
   and across restarts — the run_log's start time churns on every re-eval, so
   it can't be used for this. *)

type t = {
  started_at : float;
  completed_at : float option;
}

let path dir = Fpath.(dir / "run_timing.json")

let to_json t : Yojson.Safe.t =
  `Assoc (("started_at", `Float t.started_at)
          :: (match t.completed_at with
              | Some c -> [ ("completed_at", `Float c) ]
              | None -> []))

let of_json : Yojson.Safe.t -> t option = function
  | `Assoc a ->
    let num k = match List.assoc_opt k a with
      | Some (`Float f) -> Some f
      | Some (`Int n) -> Some (float_of_int n)
      | _ -> None in
    (match num "started_at" with
     | Some started_at -> Some { started_at; completed_at = num "completed_at" }
     | None -> None)
  | _ -> None

let read dir =
  let p = Fpath.to_string (path dir) in
  if not (Sys.file_exists p) then None
  else match Yojson.Safe.from_file p with
    | exception _ -> None
    | json -> of_json json

let write dir t =
  let p = Fpath.to_string (path dir) in
  let tmp = p ^ ".tmp" in
  let oc = open_out tmp in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc (Yojson.Safe.to_string (to_json t));
    output_char oc '\n');
  Sys.rename tmp p

(* Record the start of a snapshot's build, once. No-op if already recorded
   (a later re-evaluation, or a snapshot carried over from a prior daemon
   session, must keep its original start). *)
let record_start ~dir =
  match read dir with
  | Some _ -> ()
  | None -> write dir { started_at = Unix.gettimeofday (); completed_at = None }

(* Record completion (once) and return the build duration in seconds:
   [completed_at - started_at]. Stable on repeat calls — the first
   completion's timestamp is kept, so restarts and re-fires don't inflate
   it. Returns [None] only if no start was ever recorded (shouldn't happen:
   {!record_start} runs when the snapshot enters the pipeline). *)
let duration ~dir : float option =
  match read dir with
  | None -> None
  | Some { started_at; completed_at = Some c } -> Some (c -. started_at)
  | Some ({ started_at; completed_at = None } as t) ->
    let now = Unix.gettimeofday () in
    write dir { t with completed_at = Some now };
    Some (now -. started_at)
