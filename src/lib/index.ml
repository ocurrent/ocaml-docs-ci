open Current.Syntax
module Db = Current.Db

(* Lifted from ocaml-ci *)
module Migration = struct
  open Lwt.Infix

  let ( >>!= ) = Lwt_result.Infix.( >>= )

  type t = string

  let id = "ocaml-ci-db"

  module Key = struct
    type t = float

    let digest = Float.to_string
    let pp f t = Fmt.pf f "Date %f" t
  end

  module Value = Current.Unit

  let to_current_error = function
    | Ok () -> Lwt_result.return ()
    | Error err ->
        let msg =
          match err with
          | `Unknown_driver s ->
              Printf.sprintf "omigrate: unknown driver (%s)" s
          | `Bad_uri s -> Printf.sprintf "omigrate: bad uri (%s)" s
          | `Invalid_source s ->
              Printf.sprintf "omigrate: invalid source (%s)" s
        in
        Lwt_result.fail (`Msg msg)

  let to_lwt_exn = function
    | Ok () -> Lwt_result.return ()
    | Error err ->
        let msg =
          match err with
          | `Unknown_driver s ->
              Printf.sprintf "omigrate: unknown driver (%s)" s
          | `Bad_uri s -> Printf.sprintf "omigrate: bad uri (%s)" s
          | `Invalid_source s ->
              Printf.sprintf "omigrate: invalid source (%s)" s
        in
        Lwt_result.fail (failwith msg)

  let migrate source =
    let db_dir = Current.state_dir "db" in
    let db_path = Fpath.(to_string (db_dir / "sqlite.db")) in
    let database = Uri.(make ~scheme:"sqlite3" ~path:db_path () |> to_string) in
    Omigrate.create ~database >>!= fun () -> Omigrate.up ~source ~database ()

  let build source job _date =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current.Job.log job "Running migration from migrations/";
    migrate source >>= to_current_error

  let pp = Key.pp
  let auto_cancel = true

  (* Functions for a test purpose *)

  let init () =
    let source =
      let pwd = Fpath.v (Sys.getcwd ()) in
      Fpath.(to_string (pwd / "migrations"))
    in
    Printf.printf "Migration.init %s" source;
    migrate source >>= to_lwt_exn |> Lwt_result.get_exn
end

module Migration_cache = Current_cache.Make (Migration)

let migrate path =
  Current.component "migrations"
  |> let> date = Current.return (Unix.time ()) in
     Migration_cache.get path date

let state_to_int = function Monitor.Failed -> 0 | Running -> 1 | Done -> 2

let int_to_state = function
  | 0 -> Ok Monitor.Failed
  | 1 -> Ok Running
  | 2 -> Ok Done
  | _ -> Error "Unrecognised status: %d"

type t = { record_package : Sqlite3.stmt; record_pipeline : Sqlite3.stmt }

let db =
  lazy
    (let db = Lazy.force Current.Db.v in
     Current_cache.Db.init ();

     let record_package =
       Sqlite3.prepare db
         "INSERT OR REPLACE INTO docs_ci_package_index (name, version, \
          step_list, status, pipeline_id) VALUES (?, ?, ?, ?, ?)"
     and record_pipeline =
       Sqlite3.prepare db
         "INSERT INTO docs_ci_pipeline_index (epoch_html, epoch_linked, \
          voodoo_do, voodoo_gen, voodoo_prep) VALUES (?, ?, ?, ?, ?) returning \
          id"
     in
     { record_package; record_pipeline })

let init () = Lwt.map (fun () -> ignore (Lazy.force db)) (Migration.init ())

let record package pipeline_id package_status step_list =
  let package_name = Package.opam package |> OpamPackage.name_to_string in
  let package_version = Package.opam package |> OpamPackage.version_to_string in
  let step_list_string =
    step_list |> Monitor.steps_list_to_yojson |> Yojson.Safe.to_string
  in

  let t = Lazy.force db in
  Db.exec t.record_package
    Sqlite3.Data.
      [
        TEXT package_name;
        TEXT package_version;
        TEXT step_list_string;
        INT (state_to_int package_status |> Int64.of_int);
        INT (pipeline_id |> Int64.of_int);
      ]

let record_new_pipeline ~voodoo_do_commit ~voodoo_gen_commit ~voodoo_prep_commit
    ~epoch_html ~epoch_linked =
  let t = Lazy.force db in
  match
    Db.query_one t.record_pipeline
      Sqlite3.Data.
        [
          TEXT epoch_html;
          TEXT epoch_linked;
          TEXT voodoo_do_commit;
          TEXT voodoo_gen_commit;
          TEXT voodoo_prep_commit;
        ]
  with
  | Sqlite3.Data.[ INT pipeline_id ] -> Ok pipeline_id
  | _ -> Error "Failed to record pipeline."
