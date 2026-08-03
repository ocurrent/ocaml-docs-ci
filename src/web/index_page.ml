(** The site index ("/" and "/index.html").

    Shadows Current_web's stock index page: the [Routes] router is
    first-match-wins, so registering this ahead of [Current_web.routes engine]
    replaces the default. It renders a short description of the service, then
    the same pipeline diagram, engine result and confirmation-threshold settings
    as the stock page (all rebuilt from the public API — [Current_web] doesn't
    expose its own index resource). *)

open Tyxml.Html
module Context = Current_web.Context
module Resource = Current_web.Resource

let render_result = function
  | Ok () -> [ txt "Success!" ]
  | Error (`Active `Waiting_for_confirmation) ->
      [ txt "Waiting for confirmation..." ]
  | Error (`Active `Ready) -> [ txt "Ready..." ]
  | Error (`Active `Running) -> [ txt "Running..." ]
  | Error (`Msg msg) -> [ txt ("ERROR: " ^ msg) ]

let settings ctx config =
  let selected = Current.Config.get_confirm config in
  let levels =
    Current.Level.values
    |> List.map @@ fun level ->
       let s = Current.Level.to_string level in
       let msg = Fmt.str "Confirm if level >= %s" s in
       let sel = if selected = Some level then [ a_selected () ] else [] in
       option ~a:(a_value s :: sel) (txt msg)
  in
  let csrf = Context.csrf ctx in
  form
    ~a:[ a_action "/set/confirm"; a_method `Post; a_class [ "settings-form" ] ]
    [
      select
        ~a:[ a_name "level" ]
        (let sel = if selected = None then [ a_selected () ] else [] in
         option ~a:(a_value "none" :: sel) (txt "No confirmation required")
         :: List.rev levels);
      input ~a:[ a_name "csrf"; a_input_type `Hidden; a_value csrf ] ();
      input ~a:[ a_input_type `Submit; a_value "Submit" ] ();
    ]

let about =
  div
    [
      h2 [ txt "ocaml-docs-ci" ];
      p
        [
          txt "This service builds the HTML documentation for the packages in ";
          a
            ~a:[ a_href "https://github.com/ocaml/opam-repository" ]
            [ txt "opam-repository" ];
          txt ", as published on ";
          a ~a:[ a_href "https://ocaml.org/packages" ] [ txt "ocaml.org" ];
          txt
            ". For every package version it solves a dependency universe, \
             builds the package, and generates its documentation with ";
          a ~a:[ a_href "https://github.com/ocaml/odoc" ] [ txt "odoc" ];
          txt
            ". When a package builds in several universes, the richest one is \
             \u{201c}blessed\u{201d} and its documentation is the one \
             published.";
        ];
      p
        [
          txt "The ";
          a ~a:[ a_href "/profiles" ] [ txt "Profiles" ];
          txt
            " dashboard tracks build status per profile: snapshots over time, \
             per-package and per-universe detail, and build logs. The pipeline \
             below is the live OCurrent view of the current run; see also the ";
          a ~a:[ a_href "/jobs" ] [ txt "jobs" ];
          txt " page. Source code and issues: ";
          a
            ~a:[ a_href "https://github.com/ocurrent/ocaml-docs-ci" ]
            [ txt "ocurrent/ocaml-docs-ci" ];
          txt ".";
        ];
    ]

let r ~engine =
  object
    inherit Resource.t
    val! can_get = `Viewer

    method! private get ctx =
      let uri = Cohttp.Request.uri (Context.request ctx) in
      let config = Current.Engine.config engine in
      let { Current.Engine.value; jobs = _ } = Current.Engine.state engine in
      let verbatim_query = Uri.verbatim_query uri in
      let path = "/pipeline.svg?" ^ Option.value verbatim_query ~default:"" in
      Context.respond_ok ctx
        [
          about;
          h2 [ txt "Pipeline" ];
          div [ object_ ~a:[ a_data path ] [ txt "Pipeline diagram" ] ];
          h2 [ txt "Result" ];
          p (render_result value);
          h2 [ txt "Settings" ];
          settings ctx config;
        ]
  end
