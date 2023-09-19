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

type t = {
  record_package : Sqlite3.stmt;
  record_pipeline : Sqlite3.stmt;
  get_recent_pipeline_ids : Sqlite3.stmt;
  get_packages_by_status : Sqlite3.stmt;
  get_package_status : Sqlite3.stmt;
  get_package_status_by_name : Sqlite3.stmt;
  get_pipeline_data : Sqlite3.stmt;
}

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
     and get_recent_pipeline_ids =
       Sqlite3.prepare db
         "SELECT id FROM docs_ci_pipeline_index ORDER BY id DESC LIMIT 2"
     and get_packages_by_status =
       Sqlite3.prepare db
         "SELECT name, version FROM docs_ci_package_index WHERE status = ? AND \
          pipeline_id = ?"
     and get_package_status =
       Sqlite3.prepare db
         "SELECT status FROM docs_ci_package_index WHERE name = ? AND version \
          = ? AND pipeline_id = ?"
     and get_package_status_by_name =
       Sqlite3.prepare db
         "SELECT version, status FROM docs_ci_package_index WHERE name = ? AND \
          pipeline_id = ?"
     and get_pipeline_data =
       Sqlite3.prepare db
         "SELECT epoch_html, epoch_linked, voodoo_do, voodoo_gen, voodoo_prep \
          FROM docs_ci_pipeline_index WHERE id = ?"
     in

     {
       record_package;
       record_pipeline;
       get_recent_pipeline_ids;
       get_packages_by_status;
       get_package_status;
       get_package_status_by_name;
       get_pipeline_data;
     })

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

let get_recent_pipeline_ids t =
  let recent_pipeline_ids =
    Db.query t.get_recent_pipeline_ids []
    |> List.map @@ function
       | Sqlite3.Data.[ INT latest ] -> latest
       | row ->
           Fmt.failwith "get_recent_pipeline_ids: invalid row %a" Db.dump_row
             row
  in
  match recent_pipeline_ids with
  | [ latest ] -> Some (latest, latest) (* only one pipeline recorded *)
  | [ latest; latest_but_one ] -> Some (latest, latest_but_one)
  | _ ->
      Fmt.pr "FAILING: %a" Fmt.(list int64) recent_pipeline_ids;
      None

let get_packages_by_status state pipeline_id =
  let t = Lazy.force db in
  let status = Int64.of_int @@ state_to_int state in
  Db.query t.get_packages_by_status Sqlite3.Data.[ INT status; INT pipeline_id ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT name; TEXT version ] -> (name, version)
     | row ->
         Fmt.failwith "get_packages_by_status: invalid row %a" Db.dump_row row

let get_package_status ~name ~version ~pipeline_id =
  let t = Lazy.force db in
  let result =
    Db.query t.get_package_status
      Sqlite3.Data.[ TEXT name; TEXT version; INT pipeline_id ]
    |> List.map @@ function
       | Sqlite3.Data.[ INT status ] -> status
       | row ->
           Fmt.failwith "get_package_status: invalid row %a" Db.dump_row row
  in
  match result with
  | [ status ] -> Some (int_to_state @@ Int64.to_int status)
  | _ -> None

type pipeline_counts = {
  failed_count : int;
  running_count : int;
  passed_count : int;
}

type pipeline_data = {
  epoch_html : string;
  epoch_linked : string;
  voodoo_do : string;
  voodoo_gen : string;
  voodoo_prep : string;
}

let get_pipeline_counts pipeline_id =
  let failed_count =
    get_packages_by_status Monitor.Failed pipeline_id |> List.length
  in
  let running_count =
    get_packages_by_status Monitor.Running pipeline_id |> List.length
  in
  let passed_count =
    get_packages_by_status Monitor.Done pipeline_id |> List.length
  in
  { failed_count; running_count; passed_count }

let get_pipeline_data pipeline_id =
  let t = Lazy.force db in
  let result =
    Db.query t.get_pipeline_data Sqlite3.Data.[ INT pipeline_id ]
    |> List.map @@ function
       | Sqlite3.Data.
           [
             TEXT epoch_html;
             TEXT epoch_linked;
             TEXT voodoo_do;
             TEXT voodoo_gen;
             TEXT voodoo_prep;
           ] ->
           (epoch_html, epoch_linked, voodoo_do, voodoo_gen, voodoo_prep)
       | row -> Fmt.failwith "get_pipeline_data: invalid row %a" Db.dump_row row
  in
  match result with
  | [ (epoch_html, epoch_linked, voodoo_do, voodoo_gen, voodoo_prep) ] ->
      Some { epoch_html; epoch_linked; voodoo_do; voodoo_gen; voodoo_prep }
  | _ -> None

(* packages - (name, version) that are failing in the latest pipeline that are passing in the latest but one *)
let get_pipeline_diff ~pipeline_id_latest ~pipeline_id_latest_but_one =
  let failing_packages_in_latest =
    get_packages_by_status Monitor.Failed pipeline_id_latest
  in
  List.filter
    (function
      | name, version ->
          let status =
            get_package_status ~name ~version
              ~pipeline_id:pipeline_id_latest_but_one
          in
          status = Some (Ok Monitor.Done) || status = Some (Ok Monitor.Running))
    failing_packages_in_latest

let get_package_status_by_name name pipeline_id =
  let t = Lazy.force db in
  Db.query t.get_package_status_by_name
    Sqlite3.Data.[ TEXT name; INT pipeline_id ]
  |> List.map @@ function
     | Sqlite3.Data.[ TEXT version; INT status ] ->
         (version, int_to_state @@ Int64.to_int status)
     | row ->
         Fmt.failwith "get_packages_by_status: invalid row %a" Db.dump_row row
