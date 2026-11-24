(** Export the module paths of every package's docs after each completed run.

    Writes [Day11_doc.Doc_modules.filename] at the root of the epoch's [html/]
    directory, which the web front end serves as
    [/profiles/<profile>/docs/doc-modules.jsonl.gz] once the epoch is live.
    ocaml.org fetches it for its "which package provides module X" search.

    Docs land in the epoch incrementally, as each package is built, so the
    export is regenerated whenever a run completes: the op's value is the run
    id. Its output only changes when the docs do, so most runs leave the file,
    and its ETag, untouched.

    If [html-live] points at another epoch (a new toolchain's epoch is being
    built and not yet promoted), that epoch no longer changes; it gets an export
    once, if it has none. *)

let src = Logs.Src.create "docs-ci.doc-modules" ~doc:"Doc modules export"

module Log = (val Logs.src_log src)

module Op = struct
  type t = No_context

  let id = "day11-doc-modules-export"

  module Key = struct
    type t = {
      base_dir : string;
      epoch_hash : string;
      max_depth : int;
      max_paths : int;
    }

    (* The limits are part of the key, so changing them re-exports on the
       next completed run. *)
    let digest { base_dir; epoch_hash; max_depth; max_paths } =
      Printf.sprintf "%s\n%s\n%d\n%d" base_dir epoch_hash max_depth max_paths
  end

  module Value = Current.String
  module Outcome = Current.Unit

  let auto_cancel = false

  let pp f ((k : Key.t), run_id) =
    Fmt.pf f "export doc modules of epoch %s after run %s"
      (String.sub k.epoch_hash 0 (min 12 (String.length k.epoch_hash)))
      run_id

  let publish No_context job (key : Key.t) run_id =
    let open Lwt.Syntax in
    let* () = Current.Job.start job ~level:Current.Level.Harmless in
    let base_dir = Fpath.v key.base_dir in
    let html_dir epoch_dir = Fpath.(epoch_dir / "html") in
    let building = html_dir Fpath.(base_dir / ("epoch-" ^ key.epoch_hash)) in
    let has_export dir =
      Sys.file_exists
        (Fpath.to_string (Fpath.add_seg dir Day11_doc.Doc_modules.filename))
    in
    let live =
      match Day11_lib.Epoch.current ~base_dir with
      | Some e
        when e.hash <> key.epoch_hash && not (has_export (html_dir e.dir)) ->
          [ html_dir e.dir ]
      | Some _ | None -> []
    in
    Current.Job.log job "Run %s complete" run_id;
    (* Reading a few thousand sidebars (some of them tens of megabytes) takes
       a while: do it in a systhread, off the engine's event loop. *)
    let+ results =
      Lwt_eio.run_eio (fun () ->
          Eio_unix.run_in_systhread (fun () ->
              List.map
                (fun dir ->
                  ( dir,
                    Day11_doc.Doc_modules.write ~max_depth:key.max_depth
                      ~max_paths:key.max_paths dir ))
                (building :: live)))
    in
    List.fold_left
      (fun acc (dir, r) ->
        match r with
        | Ok (w, n) ->
            Current.Job.log job "%a/%s: %d packages, %s" Fpath.pp dir
              Day11_doc.Doc_modules.filename n
              (match w with `Written -> "written" | `Unchanged -> "unchanged");
            acc
        | Error e ->
            Current.Job.log job "%a: export failed: %s" Fpath.pp dir e;
            Error (`Msg e))
      (Ok ()) results
end

module Export = Current_cache.Output (Op)

(** [export ~max_depth ~max_paths ~base_dir ~epoch_hash run_id] writes the
    doc-modules export of [base_dir/epoch-<epoch_hash>/html] each time [run_id],
    the id of the last completed run, changes. [max_depth] and [max_paths] are
    as for {!Day11_doc.Doc_modules.write}. *)
let export ~max_depth ~max_paths ~base_dir ~epoch_hash
    (run_id : string Current.t) : unit Current.t =
  let open Current.Syntax in
  Current.component "doc modules"
  |>
  let> run_id in
  Export.set Op.No_context
    {
      Op.Key.base_dir = Fpath.to_string base_dir;
      epoch_hash;
      max_depth;
      max_paths;
    }
    run_id
