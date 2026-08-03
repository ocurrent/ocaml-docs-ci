let contains_substring_ci ~pattern text =
  let pat = String.lowercase_ascii pattern in
  let pat_len = String.length pat in
  let text_len = String.length text in
  if pat_len > text_len then false
  else
    let rec check i =
      if i > text_len - pat_len then false
      else if String.lowercase_ascii (String.sub text i pat_len) = pat then true
      else check (i + 1)
    in
    check 0

let matches_any patterns text =
  List.exists (fun pat -> contains_substring_ci ~pattern:pat text) patterns

let extract_compiler_from_deps json =
  let open Yojson.Safe.Util in
  let deps =
    try json |> member "deps" |> to_list |> List.map to_string with _ -> []
  in
  let compiler_pkg =
    List.find_opt
      (fun dep ->
        let name =
          try String.sub dep 0 (String.index dep '.') with Not_found -> dep
        in
        name = "ocaml-base-compiler" || name = "ocaml-variants")
      deps
  in
  match compiler_pkg with
  | Some pkg -> (
      try
        let dot = String.index pkg '.' in
        String.sub pkg (dot + 1) (String.length pkg - dot - 1)
      with Not_found -> pkg)
  | None -> ""

let classify_build_log log_content =
  let transient_patterns =
    [
      "No space left on device";
      "Connection timed out";
      "Could not resolve host";
      "Temporary failure in name resolution";
      "Network is unreachable";
    ]
  in
  let depext_patterns =
    [
      "Unable to locate package";
      "is not available";
      "unmet dependencies";
      "dpkg: dependency problems";
    ]
  in
  if matches_any transient_patterns log_content then
    ( "failure",
      "transient_failure",
      Some "Transient infrastructure failure detected in build log" )
  else if matches_any depext_patterns log_content then
    ( "failure",
      "depext_unavailable",
      Some "Missing system dependency detected in build log" )
  else ("failure", "build_failure", None)
