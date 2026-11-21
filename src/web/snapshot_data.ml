(** Machine-readable snapshot data.

    The data behind the HTML snapshots and diff pages, served raw, so that a
    client can follow the pipeline incrementally: compute its own diff between
    two complete snapshots and refetch only the packages whose status changed,
    instead of revalidating every package's documentation.

    - [/profiles/<name>/snapshots.json]: every snapshot of the profile, newest
      first, as [[{"key": _, "created": _, "complete": _}, ...]]. [created] is
      the ISO-8601 UTC creation time (see {!Pages.snapshot_created}); [complete]
      is true once the run finished and wrote [final_status.json] (see
      {!Pages.snapshot_completed}).
    - [/profiles/<name>/snapshots/<key>/final_status.json]: that file as written
      by {!Day11_lib.Status_index.write_final_status}, an object from
      ["name.version"] to the package's final status category. 404 while the run
      is unfinished, or if it was superseded before finishing. *)

module Resource = Current_web.Resource

let json_headers = Cohttp.Header.init_with "Content-Type" "application/json"

let respond_json body =
  Cohttp_lwt_unix.Server.respond_string ~headers:json_headers ~status:`OK ~body
    ()

(* Profile names and snapshot keys become path components: accept only the
   characters they are made of, so that "..", "." or an encoded separator never
   reaches the filesystem. *)
let valid_segment s =
  s <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

let snapshots ~(ctx : Pages.ctx) name =
  object
    inherit Resource.t
    val! can_get = `Viewer

    method! private get web_ctx =
      if not (valid_segment name) then
        Current_web.Context.respond_error web_ctx `Not_found "no such profile"
      else
        let entries =
          Pages.list_snapshots_newest_first ctx name
          |> List.map (fun dir ->
              `Assoc
                [
                  ("key", `String (Fpath.basename dir));
                  ("created", `String (Pages.snapshot_created dir));
                  ("complete", `Bool (Pages.snapshot_completed dir));
                ])
        in
        respond_json (Yojson.Safe.to_string (`List entries) ^ "\n")
  end

let final_status ~(ctx : Pages.ctx) name key =
  object
    inherit Resource.t
    val! can_get = `Viewer

    method! private get web_ctx =
      if not (valid_segment name && valid_segment key) then
        Current_web.Context.respond_error web_ctx `Not_found "no such snapshot"
      else
        let path =
          Fpath.(Pages.snapshots_base ctx name / key / "final_status.json")
        in
        match Bos.OS.File.read path with
        | Ok body -> respond_json body
        | Error _ ->
            Current_web.Context.respond_error web_ctx `Not_found
              (Printf.sprintf
                 "snapshot %s has no final statuses: it does not exist, its \
                  run has not finished, or it was superseded"
                 key)
  end
