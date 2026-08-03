type doc_phase = Doc_all | Doc_compile_only | Doc_link_only

type doc_result =
  | Doc_success of { html_path : string; blessed : bool }
  | Doc_skipped
  | Doc_failure of string

let phase_to_string = function
  | Doc_all -> "all"
  | Doc_compile_only -> "compile-only"
  | Doc_link_only -> "link-and-gen"

let doc_result_to_yojson = function
  | Doc_success { html_path; blessed } ->
      `Assoc
        [
          ("status", `String "success");
          ("html_path", `String html_path);
          ("blessed", `Bool blessed);
        ]
  | Doc_skipped -> `Assoc [ ("status", `String "skipped") ]
  | Doc_failure msg ->
      `Assoc [ ("status", `String "failure"); ("message", `String msg) ]

let doc_result_of_yojson json =
  try
    let open Yojson.Safe.Util in
    match json |> member "status" |> to_string with
    | "success" ->
        let html_path = json |> member "html_path" |> to_string in
        let blessed = json |> member "blessed" |> to_bool in
        Ok (Doc_success { html_path; blessed })
    | "skipped" -> Ok Doc_skipped
    | "failure" ->
        let msg = json |> member "message" |> to_string in
        Ok (Doc_failure msg)
    | s -> Error ("unknown doc_result status: " ^ s)
  with exn -> Error (Printexc.to_string exn)
