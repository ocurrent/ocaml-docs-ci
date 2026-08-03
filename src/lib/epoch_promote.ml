(** Manual epoch promotion as a [Dangerous]-level OCurrent output.

    The doc pipeline builds each profile's HTML into a versioned [epoch-<hash>/]
    dir; the live site is served through a [html-live] symlink. Promotion swaps
    that symlink to a freshly-built epoch.

    This is deliberately a {!Current.Level.Dangerous} op: with the engine's
    confirmation threshold set below [Dangerous] (via [Current.Config]), it
    surfaces in the web UI as a node awaiting a manual click before it runs — so
    a rebuilt epoch goes live only when a human promotes it, exactly as the old
    ocaml-docs-ci did. Each distinct epoch hash is a separate output, so a new
    epoch reappears as a fresh "needs confirmation" node. *)

let src = Logs.Src.create "docs-ci.epoch-promote" ~doc:"Epoch promotion"

module Log = (val Logs.src_log src)

module Op = struct
  type t = No_context

  let id = "day11-epoch-promote"

  module Key = struct
    type t = { base_dir : string; epoch_hash : string }

    (* Key on (base_dir, epoch_hash) so each epoch is its own output:
       a newly-built epoch surfaces as a fresh node to confirm. *)
    let digest { base_dir; epoch_hash } =
      Printf.sprintf "%s\n%s" base_dir epoch_hash
  end

  module Value = Current.Unit
  module Outcome = Current.Unit

  let auto_cancel = false

  let pp f ((k : Key.t), ()) =
    Fmt.pf f "promote epoch %s"
      (String.sub k.epoch_hash 0 (min 12 (String.length k.epoch_hash)))

  let publish No_context job (key : Key.t) () =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Dangerous in
    let base_dir = Fpath.v key.base_dir in
    let epoch =
      {
        Day11_lib.Epoch.hash = key.epoch_hash;
        dir = Fpath.(base_dir / ("epoch-" ^ key.epoch_hash));
      }
    in
    Lwt.return
      (try
         (* Only the symlink swap — instant. Reclaiming old epoch dirs is
            a separate, decoupled action (see {!Epoch_gc}): each epoch dir
            can hold millions of files, and deleting one here would block
            the engine for the whole (multi-minute) delete. *)
         Day11_lib.Epoch.promote ~base_dir epoch;
         Current.Job.log job "Promoted epoch %s -> %a/html-live" key.epoch_hash
           Fpath.pp base_dir;
         Ok ()
       with exn ->
         Error
           (`Msg
              (Printf.sprintf "epoch promote failed: %s"
                 (Printexc.to_string exn))))
end

module Promote = Current_cache.Output (Op)

(** [promote ~base_dir ~epoch_hash] is a [Dangerous] OCurrent node that, once
    confirmed in the web UI, points [base_dir/html-live] at
    [base_dir/epoch-<epoch_hash>/html]. *)
let promote ~base_dir ~epoch_hash : unit Current.t =
  let open Current.Syntax in
  Current.component "promote epoch %s"
    (String.sub epoch_hash 0 (min 12 (String.length epoch_hash)))
  |>
  let> () = Current.return () in
  Promote.set Op.No_context
    { Op.Key.base_dir = Fpath.to_string base_dir; epoch_hash }
    ()
