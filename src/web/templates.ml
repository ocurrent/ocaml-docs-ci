(** Reusable in-page helpers for the dashboard pages. Pages render via
    {!Current_web.Context.respond_ok}, which already wraps the body in the
    site-wide chrome (the OCurrent nav with our pages listed via their
    [nav_link] methods). What lives here is the smaller bits that need to look
    the same across pages — breadcrumbs, status badges, short-SHA rendering,
    etc. *)

open Tyxml.Html

(** Inline stylesheet. Loaded as a [<style>] block at the top of each page body
    — small enough to inline, no static-asset plumbing to worry about. *)
let style =
  {|
  .crumbs { color: #34495e; font-size: 0.9em;
            margin: 0 0 1em 0; padding-bottom: 0.4em;
            border-bottom: 1px solid #ecf0f1; }
  .crumbs a { color: #2c3e50; text-decoration: none; }
  .crumbs a:hover { text-decoration: underline; }
  table.data { border-collapse: collapse; margin: 0.6em 0; }
  table.data th, table.data td {
    padding: 0.3em 0.7em; text-align: left;
    border-bottom: 1px solid #ecf0f1;
  }
  table.data th { background: #f8f9fa; font-weight: 600; }
  .ok { color: #27ae60; }
  .fail { color: #c0392b; }
  .cascade { color: #d68910; }
  .pending { color: #7f8c8d; font-style: italic; }
  .warn { background: #fef9e7; border-left: 3px solid #f1c40f;
          padding: 0.4em 0.7em; margin: 0.5em 0; }
  .error-box { background: #fdedec; border-left: 3px solid #c0392b;
               padding: 0.4em 0.7em; margin: 0.5em 0; }
  .sha { font-family: ui-monospace, "SF Mono", Menlo, monospace;
         font-size: 0.9em; color: #7f8c8d; }
  .pager { margin-top: 1em; font-size: 0.9em; }
  table.data details summary { cursor: pointer; color: #2c3e50; }
  table.data details ul { margin: 0.3em 0; padding-left: 1.2em;
                          columns: 2; font-size: 0.9em; }
  table.data details[open] { min-width: 22em; }
  #bd-cmp-result { margin: 0.5em 0; }
|}

let style_block = Tyxml.Html.style [ Unsafe.data style ]

(** Status-aware text. Maps a [history.jsonl] / [build.jsonl] status string to a
    colour-coded [<span>]. *)
let status_span s =
  let cls =
    match s with
    | "ok" | "success" -> "ok"
    | "fail" | "failure" | "error" -> "fail"
    | "cascade" -> "cascade"
    | "pending" | "in_progress" -> "pending"
    | _ -> ""
  in
  span ~a:[ a_class [ cls ] ] [ txt s ]

(** First 12 characters of a SHA. *)
let short_sha s = if String.length s <= 12 then s else String.sub s 0 12

(** Render a SHA as monospace small text. *)
let sha_span s = span ~a:[ a_class [ "sha" ] ] [ txt (short_sha s) ]

(** Breadcrumb element. [parts] is a list of [(href_opt, text)] — [None] for the
    current (non-link) page. *)
let breadcrumbs parts =
  let sep = txt " › " in
  let render = function
    | None, text -> txt text
    | Some href, text -> a ~a:[ a_href href ] [ txt text ]
  in
  let rec interleave = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: rest -> x :: sep :: interleave rest
  in
  div ~a:[ a_class [ "crumbs" ] ] (interleave (List.map render parts))
