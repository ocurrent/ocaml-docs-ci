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

type status = [ `Failed | `Running | `Passed ] [@@deriving show]

let status_to_int = function `Failed -> 0 | `Running -> 1 | `Passed -> 2

let int_to_status = function
  | 0 -> Ok `Failed
  | 1 -> Ok `Running
  | 2 -> Ok `Passed
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
         "INSERT OR REPLACE INTO docs_ci_pipeline_index (epoch_1, epoch_2, \
          voodoo_do, voodoo_gen, voodoo_compile) VALUES (?, ?, ?, ?, ?)"
     in
     { record_package; record_pipeline })

let init () = Lwt.map (fun () -> ignore (Lazy.force db)) (Migration.init ())

let record package config ~voodoo_do_commit ~voodoo_gen_commit step_list =
  Log.info (fun f ->
      f
        "[Index] Package: %s:%s Voodoo-branch: %s Voodoo-repo: %s \
         Voodoo-do-commit: %s Voodoo-gen-commit: %s Step-list: %a"
        (Package.opam package |> OpamPackage.name_to_string)
        (Package.opam package |> OpamPackage.version_to_string)
        (Config.voodoo_branch config)
        (Config.voodoo_repo config)
        voodoo_do_commit voodoo_gen_commit
        (Format.pp_print_list Monitor.pp_step)
        step_list);
  ()
