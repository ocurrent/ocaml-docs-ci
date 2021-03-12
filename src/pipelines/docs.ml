open Docs_ci_lib

module OpamPackage = struct
  include OpamPackage

  let pp f t = Fmt.pf f "%s" (to_string t)
end

let v ~opam () =
  let open Docs in
  let open Current.Syntax in
  let* opam = opam in
  track ~filter:[ "uri" ] (Current.return opam)
  |> Current.list_map (module OpamPackage) (solve ~opam)
  |> Current.list_map (module Package) (build_and_prep ~opam)
  |> assemble_and_link
