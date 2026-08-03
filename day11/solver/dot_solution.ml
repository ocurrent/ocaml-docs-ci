let to_string pkgs =
  let quoted package = "\"" ^ OpamPackage.to_string package ^ "\"" in
  let graph =
    OpamPackage.Map.to_list pkgs
    |> List.filter_map (fun (pkg, deps) ->
        match OpamPackage.Set.to_list deps with
        | [] -> None
        | [ p ] -> Some ("  " ^ quoted pkg ^ " -> " ^ quoted p ^ ";")
        | lst ->
            Some
              ("  "
              ^ quoted pkg
              ^ " -> {"
              ^ (lst |> List.map quoted |> String.concat " ")
              ^ "}"))
    |> String.concat "\n"
  in
  "digraph opam {\n" ^ graph ^ "\n}\n"

let save path pkgs =
  try
    Bos.OS.File.write path (to_string pkgs)
    |> Result.map_error (fun (`Msg m) -> `Msg m)
  with exn ->
    Rresult.R.error_msgf "Dot_solution.save: %s" (Printexc.to_string exn)
