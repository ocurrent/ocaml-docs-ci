type t = Deps.t

let to_json pkgs =
  `Assoc
    (OpamPackage.Map.fold
       (fun pkg deps lst ->
         let dep_strs =
           OpamPackage.Set.to_list_map
             (fun p -> `String (OpamPackage.to_string p))
             deps
         in
         (OpamPackage.to_string pkg, `List dep_strs) :: lst)
       pkgs [])

let of_json json =
  try
    let open Yojson.Safe.Util in
    Ok
      (json
      |> to_assoc
      |> List.fold_left
           (fun acc (s, l) ->
             let pkg = OpamPackage.of_string s in
             let deps =
               l
               |> to_list
               |> List.map (fun s -> s |> to_string |> OpamPackage.of_string)
               |> OpamPackage.Set.of_list
             in
             OpamPackage.Map.add pkg deps acc)
           OpamPackage.Map.empty)
  with exn ->
    Rresult.R.error_msgf "Solution_json.of_json: %s" (Printexc.to_string exn)

let save path solution =
  try
    Yojson.Safe.to_file (Fpath.to_string path) (to_json solution);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Solution_json.save %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let load path =
  try
    let json = Yojson.Safe.from_file (Fpath.to_string path) in
    of_json json
  with exn ->
    Rresult.R.error_msgf "Solution_json.load %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let to_string solution = Yojson.Safe.to_string (to_json solution)

let of_string str =
  try
    let json = Yojson.Safe.from_string str in
    of_json json
  with exn ->
    Rresult.R.error_msgf "Solution_json.of_string: %s" (Printexc.to_string exn)
