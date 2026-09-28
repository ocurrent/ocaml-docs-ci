(** Reconcile an epoch's HTML against the doc layers its profile plans.

    Doc layers keep their rendered HTML ({!Day11_doc.Html_publish}), and a fresh
    build publishes into the epoch of the profile that dispatched it. A layer
    that another profile built first is a cache hit here — OCurrent keys
    [day11-node] on the layer hash alone, so this profile never runs it — and
    nothing publishes it into this profile's epoch. This op closes that gap:
    once a run completes, it links every planned doc-all/link layer's pages that
    the epoch doesn't have yet. Units carry a marker naming their layer, so a
    pass over an up-to-date epoch is one small read per package-version.

    It also self-heals: a unit deleted from the epoch (by hand, or a crash
    mid-swap) is re-linked on the next run, whereas a per-layer node would stay
    cached as done.

    Keyed on (epoch html dir, run id), so it runs once per completed run. The
    layer list rides in the op's context, keeping the key small. *)

let src = Logs.Src.create "docs-ci.epoch-publish" ~doc:"Epoch HTML reconcile"

module Log = (val Logs.src_log src)

module Op = struct
  type t = {
    env : Eio_unix.Stdenv.base;
    layers : (string * Fpath.t) list;
    support_root : Fpath.t;
  }

  let id = "day11-epoch-publish"

  module Key = struct
    type t = { epoch_html : string; run_id : string }

    let digest { epoch_html; run_id } =
      Printf.sprintf "%s\n%s" epoch_html run_id
  end

  module Value = Current.Unit

  let auto_cancel = true

  let pp f (k : Key.t) =
    Fmt.pf f "publish html into %s (run %s)" k.epoch_html k.run_id

  let build (ctx : t) job (key : Key.t) =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Harmless in
    Current.Job.log job "Reconciling %d doc layer(s) into %s"
      (List.length ctx.layers) key.epoch_html;
    let t0 = Unix.gettimeofday () in
    (* Tens of thousands of stats/links on a large profile: keep it off the
       event loop. A systhread, not a domain — see the disk-metrics note in
       ocaml_docs_ci.ml (domains would forbid the build path's fork). *)
    let+ (st : Day11_doc.Html_publish.stats) =
      Lwt_eio.run_eio (fun () ->
          ignore ctx.env;
          Eio_unix.run_in_systhread (fun () ->
              Day11_doc.Html_publish.reconcile
                ~epoch_html:(Fpath.v key.epoch_html)
                ~support_root:ctx.support_root ctx.layers))
    in
    Current.Job.log job
      "%d layer(s): %d unit(s) linked, %d already up to date, %d layer(s) \
       without html, %d error(s) in %.1fs"
      st.layers st.units st.skipped st.no_html (List.length st.errors)
      (Unix.gettimeofday () -. t0);
    List.iteri
      (fun i (hash, msg) ->
        if i < 50 then Current.Job.log job "  %s: %s" hash msg)
      st.errors;
    if st.units > 0 then
      Log.info (fun f ->
          f "reconcile %s: linked %d unit(s)" key.epoch_html st.units);
    match st.errors with
    | [] -> Ok ()
    | _ ->
        Error
          (`Msg
             (Printf.sprintf "%d layer(s) failed to publish"
                (List.length st.errors)))
end

module Cache = Current_cache.Make (Op)

(** [reconcile ~env ~support_root ~epoch_html ~run_id layers] links the HTML of
    every [(hash, layer_dir)] in [layers] into [epoch_html]. *)
let reconcile ~env ~support_root ~epoch_html ~run_id
    (layers : (string * Fpath.t) list Current.t) : unit Current.t =
  let open Current.Syntax in
  Current.component "publish html"
  |>
  let> layers in
  Cache.get
    { Op.env; layers; support_root }
    { Op.Key.epoch_html = Fpath.to_string epoch_html; run_id }
