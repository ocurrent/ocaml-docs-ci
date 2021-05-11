open Lwt.Syntax

let respond = Lwt.map Dream.response

type scope_kind = Packages | Universes

let find_default_version state name =
  let+ versions = Docs2web.State.get_package_opt state name in
  Option.map
    (fun versions -> OpamPackage.Version.Map.max_binding versions |> fst)
    versions

let redirect ~target =
  Dream.response ~status:`Moved_Permanently ~headers:[ ("Location", target) ] ""

let not_found ~prefix _ =
  Dream.respond ~status:`Not_Found (Docs2web_pages.Notfound.v prefix ())

let packages_scope ~state kind =
  let open Docs2web_pages in
  let prefix = Docs2web.State.prefix state in
  let not_found = not_found ~prefix in
  let get_kind request =
    match kind with
    | Packages -> Package.Blessed
    | Universes -> Package.Universe (Dream.param "hash" request)
  in
  [
    Dream.get "/" (fun _ -> respond (Packages.v ~state));
    Dream.get "/index.html" (fun _ -> respond (Packages.v ~state));
    Dream.get "/**" (fun request ->
        let kind = get_kind request in
        Dream.log "%s" (Dream.path request |> String.concat "--");
        match Dream.path request with
        | [ package ] | [ package; "" ] -> (
            let name =
              try OpamPackage.Name.of_string package
              with Failure _ ->
                OpamPackage.Name.of_string "non-existent-package"
            in
            let* version = find_default_version state name in
            match version with
            | Some version ->
                let target =
                  Dream.prefix request ^ "/" ^ package ^ "/"
                  ^ OpamPackage.Version.to_string version
                  ^ "/"
                in
                Lwt.return (redirect ~target)
            | None -> not_found () )
        | [ package; version ] ->
            Lwt.return
              (redirect
                 ~target:
                   (Dream.prefix request ^ "/" ^ package ^ "/" ^ version ^ "/"))
        | package :: version :: path -> (
            let name =
              try OpamPackage.Name.of_string package
              with Failure _ ->
                OpamPackage.Name.of_string "non-existent-package"
            in
            let version =
              try OpamPackage.Version.of_string version
              with Failure _ -> OpamPackage.Version.of_string "0"
            in
            let path = String.concat "/" path in
            try respond (Package.v ~state ~kind ~name ~version ~path ()) with
            | Not_found -> not_found ()
            | Failure _ -> not_found () )
        | _ -> Dream.respond ~status:`Internal_Server_Error "");
  ]

let cache_header handler request =
  let open Lwt.Syntax in
  let+ response = handler request in
  Dream.with_header "Cache-Control" "public, max-age=3600, immutable" response

let job ~interface ~port ~api ~prefix ~polling =
  let state = Docs2web.State.v ~api ~prefix ~polling () in
  Dream.log "Ready to serve at http://localhost:%d%s" port prefix;
  Dream.serve ~interface ~port ~prefix
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> respond (Docs2web_pages.Index.v ~state));
         Dream.scope "/packages" [] (packages_scope ~state Packages);
         Dream.scope "/universes" []
           [
             Dream.get "/" (fun _ -> Dream.respond "universes");
             Dream.scope "/:hash" [] (packages_scope ~state Universes);
           ];
         Dream.scope "/static" [ cache_header ]
           [ Dream.get "/**" @@ Dream.static "static" ];
       ]
  @@ not_found ~prefix

let main interface port api prefix polling =
  let api = Uri.of_string api in
  Lwt_main.run (job ~interface ~port ~api ~prefix ~polling)

(* Command-line parsing *)

open Cmdliner

let interface =
  Arg.value
  @@ Arg.opt Arg.string "localhost"
  @@ Arg.info ~doc:"Interface to listen on" ~docv:"IF" [ "interface" ]

let port =
  Arg.value @@ Arg.opt Arg.int 8082
  @@ Arg.info ~doc:"The port on which to listen for HTTP connections."
       ~docv:"PORT" [ "port" ]

let polling =
  Arg.value @@ Arg.opt Arg.int 30
  @@ Arg.info ~doc:"Polling speed in seconds."
       ~docv:"SECS" [ "polling" ]

let api =
  Arg.value
  @@ Arg.opt Arg.string "http://localhost:8081/graphql"
  @@ Arg.info ~doc:"API endpoint" ~docv:"URL" [ "api" ]

let prefix =
  Arg.value @@ Arg.opt Arg.string "/"
  @@ Arg.info ~doc:"Server prefix" [ "prefix" ]

let cmd =
  let doc = "Docs2web: a frontend for the docs ci" in
  ( Term.(const main $ interface $ port $ api $ prefix $ polling),
    Term.info "docs2web" ~doc )

let () = Term.(exit @@ eval cmd)
