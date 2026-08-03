type t = {
  packages : OpamPackage.Set.t;
  build_deps : Deps.t;
  doc_deps : Deps.t;
  examined : OpamPackage.Name.Set.t;
}

let packages_to_json pkgs =
  `List
    (OpamPackage.Set.fold
       (fun p acc -> `String (OpamPackage.to_string p) :: acc)
       pkgs [])

let packages_of_json json =
  let open Yojson.Safe.Util in
  json
  |> to_list
  |> List.map to_string
  |> List.map OpamPackage.of_string
  |> OpamPackage.Set.of_list

let examined_to_json examined =
  `List
    (OpamPackage.Name.Set.fold
       (fun n acc -> `String (OpamPackage.Name.to_string n) :: acc)
       examined [])

let examined_of_json json =
  let open Yojson.Safe.Util in
  json
  |> to_list
  |> List.map to_string
  |> List.map OpamPackage.Name.of_string
  |> OpamPackage.Name.Set.of_list

let to_json t =
  `Assoc
    [
      ("packages", packages_to_json t.packages);
      ("build_deps", Json.to_json t.build_deps);
      ("doc_deps", Json.to_json t.doc_deps);
      ("examined", examined_to_json t.examined);
    ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let packages = json |> member "packages" |> packages_of_json in
    let build_deps = json |> member "build_deps" |> Json.of_json in
    let doc_deps = json |> member "doc_deps" |> Json.of_json in
    let examined = json |> member "examined" |> examined_of_json in
    match (build_deps, doc_deps) with
    | Ok build_deps, Ok doc_deps ->
        Ok { packages; build_deps; doc_deps; examined }
    | Error e, _ | _, Error e -> Error e
  with exn ->
    Rresult.R.error_msgf "Solve_result.of_json: %s" (Printexc.to_string exn)
