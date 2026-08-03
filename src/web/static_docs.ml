(** Per-profile static-file serving for rendered HTML.

    Profiles' [html_dir] field points at the dir voodoo writes HTML into. We
    mount it at [/profiles/<name>/docs/...] so the dashboard can deep-link into
    module pages. *)

module Resource = Current_web.Resource
module Profile = Day11_batch.Profile

let mime_of_ext path =
  match Filename.extension path with
  | ".html" | ".htm" -> "text/html; charset=utf-8"
  | ".css" -> "text/css"
  | ".js" -> "application/javascript"
  | ".json" -> "application/json"
  | ".svg" -> "image/svg+xml"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".woff" -> "font/woff"
  | ".woff2" -> "font/woff2"
  | ".ttf" -> "font/ttf"
  | ".txt" -> "text/plain; charset=utf-8"
  | _ -> "application/octet-stream"

(** Validate that [tail] resolves to a path strictly inside [root]. Rejects [..]
    traversal. Empty segments (from leading [/] or [//] runs) are dropped —
    they're benign and routes' wildcard captures sometimes include them. Returns
    [None] on suspicious input. *)
let resolve_inside ~root tail =
  if String.contains tail '\x00' then None
  else
    let parts =
      String.split_on_char '/' tail |> List.filter (fun s -> s <> "")
    in
    if List.exists (fun s -> s = "..") parts then None
    else if parts = [] then Some Fpath.(root / "index.html")
    else
      let rel = String.concat "/" parts in
      Some Fpath.(root // v rel)

let read_file path =
  try
    Some
      (let ic = open_in_bin (Fpath.to_string path) in
       Fun.protect
         ~finally:(fun () -> close_in ic)
         (fun () ->
           let len = in_channel_length ic in
           really_input_string ic len))
  with _ -> None

(** Resource for [/profiles/<name>/docs/<tail>]. Resolves [tail] relative to the
    profile's [html_dir], serves the file with a sniffed MIME type. Empty [tail]
    (i.e. just [/.../docs/]) rewrites to [index.html]. *)
let resource ~profile_dir name tail =
  object
    inherit Resource.t
    val! can_get = `Viewer

    method! private get web_ctx =
      let open Lwt.Syntax in
      let* response =
        match Profile.load ~dir:profile_dir ~name with
        | Error (`Msg e) ->
            Current_web.Context.respond_error web_ctx `Not_found
              (Printf.sprintf "no such profile: %s (%s)" name e)
        | Ok profile -> (
            match profile.html_dir with
            | None ->
                Current_web.Context.respond_error web_ctx `Not_found
                  (Printf.sprintf "profile %s has no html_dir configured" name)
            | Some root_str -> (
                (* Serve through the [html-live] symlink (the live epoch). *)
                let root = Fpath.(v root_str / "html-live") in
                let tail = if tail = "" then "index.html" else tail in
                match resolve_inside ~root tail with
                | None ->
                    Current_web.Context.respond_error web_ctx `Bad_request
                      (Printf.sprintf "bad path: %S" tail)
                | Some path -> (
                    match read_file path with
                    | None ->
                        Current_web.Context.respond_error web_ctx `Not_found
                          "file not found"
                    | Some body ->
                        let headers =
                          Cohttp.Header.init_with "Content-Type"
                            (mime_of_ext (Fpath.to_string path))
                        in
                        Cohttp_lwt_unix.Server.respond_string ~headers
                          ~status:`OK ~body ())))
      in
      Lwt.return response
  end
