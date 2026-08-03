(** Manual epoch garbage-collection as a [Dangerous]-level OCurrent output.

    Decoupled from {!Epoch_promote}: promotion only swaps the [html-live]
    symlink (instant), while reclaiming old epoch dirs is its own confirmable
    action. Each epoch dir can hold millions of files, so the deletion is
    shelled out to [rm -rf] via {!Lwt_process} — it runs as a subprocess and
    never blocks the OCurrent/Lwt event loop (an in-process [Bos.OS.Dir.delete]
    here would freeze the whole engine for the multi-minute duration of the
    delete).

    Like promotion this is a {!Current.Level.Dangerous} op keyed per
    [(base_dir, epoch_hash)], so each freshly-promotable epoch surfaces a
    matching gc node in the web UI awaiting a manual click. *)

let src = Logs.Src.create "docs-ci.epoch-gc" ~doc:"Epoch gc"

module Log = (val Logs.src_log src)

(* Keep this many most-recent epochs (plus the live one — gc never
   deletes the current symlink target), matching the old inline gc. *)
let keep = 3

module Op = struct
  type t = No_context

  let id = "day11-epoch-gc"

  module Key = struct
    type t = { base_dir : string; epoch_hash : string }

    (* Key on (base_dir, epoch_hash) so each newly-promotable epoch gets
       its own gc node, mirroring {!Epoch_promote}. The hash isn't used by
       the gc itself (which inspects live filesystem state) — it's only
       what makes a fresh confirmation node appear per epoch. *)
    let digest { base_dir; epoch_hash } =
      Printf.sprintf "%s\n%s" base_dir epoch_hash
  end

  module Value = Current.Unit
  module Outcome = Current.Unit

  let auto_cancel = false
  let pp f ((k : Key.t), ()) = Fmt.pf f "gc epochs in %s" k.base_dir

  let publish No_context job (key : Key.t) () =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Dangerous in
    let base_dir = Fpath.v key.base_dir in
    (* Compute the to-delete set from live filesystem state at run time,
       not from the key — so it always honours the current [html-live]
       target even if it moved after this node was created. *)
    let dirs = Day11_lib.Epoch.to_gc ~base_dir ~keep in
    match dirs with
    | [] ->
        Current.Job.log job
          "No old epochs to gc (keeping %d most-recent + live)" keep;
        Lwt.return (Ok ())
    | _ ->
        Current.Job.log job "Reclaiming %d old epoch dir%s" (List.length dirs)
          (if List.length dirs = 1 then "" else "s");
        let rec loop = function
          | [] -> Lwt.return (Ok ())
          | dir :: rest -> (
              let path = Fpath.to_string dir in
              Current.Job.log job "rm -rf %s" path;
              (* [rm -rf] in a subprocess: non-blocking for the event loop,
             and far faster than per-file OCaml unlink on a tree of
             millions of files. *)
              let* status =
                Lwt_process.exec ("", [| "rm"; "-rf"; "--"; path |])
              in
              match status with
              | Unix.WEXITED 0 -> loop rest
              | Unix.WEXITED n ->
                  Lwt.return
                    (Error (`Msg (Printf.sprintf "rm -rf %s exited %d" path n)))
              | Unix.WSIGNALED n | Unix.WSTOPPED n ->
                  Lwt.return
                    (Error
                       (`Msg
                          (Printf.sprintf "rm -rf %s killed by signal %d" path n)))
              )
        in
        let* result = loop dirs in
        (match result with
        | Ok () ->
            Current.Job.log job "gc complete (%d epoch dir%s removed)"
              (List.length dirs)
              (if List.length dirs = 1 then "" else "s")
        | Error _ -> ());
        Lwt.return result
end

module Gc = Current_cache.Output (Op)

(** [gc ~base_dir ~epoch_hash] is a [Dangerous] OCurrent node that, once
    confirmed in the web UI, deletes old epoch dirs under [base_dir] (keeping
    the {!keep} most-recent plus the live one). [epoch_hash] only keys the node
    so a fresh confirmation appears per promotable epoch. *)
let gc ~base_dir ~epoch_hash : unit Current.t =
  let open Current.Syntax in
  Current.component "gc epochs (after %s)"
    (String.sub epoch_hash 0 (min 12 (String.length epoch_hash)))
  |>
  let> () = Current.return () in
  Gc.set Op.No_context
    { Op.Key.base_dir = Fpath.to_string base_dir; epoch_hash }
    ()
