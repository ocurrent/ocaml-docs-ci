type t = string

let of_deps deps =
  deps
  |> OpamPackage.Set.elements
  |> List.map OpamPackage.to_string
  |> String.concat "\n"
  |> Digest.string
  |> Digest.to_hex

let dummy = ""
let equal = String.equal
let to_string t = t
let pp fmt t = Format.pp_print_string fmt t
