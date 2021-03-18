
let get_base_image package =
  let deps = Package.universe package |> Package.Universe.deps in
  let ocaml_version =
    deps
    |> List.find_opt (fun pkg -> pkg |> Package.opam |> OpamPackage.name_to_string  = "ocaml")
    |> Option.map (fun pkg -> pkg |> Package.opam |> OpamPackage.version_to_string)
    |> Option.value ~default:"4.12.0"
  in
  let base_image_version =
    match Astring.String.cuts ~sep:"." ocaml_version with
    | [ major; minor; _micro ] ->
        Format.eprintf "major: %s minor: %s\n%!" major minor;
        major ^ "." ^ minor
    | _xs -> "4.12"
  in
  Spec.make ("ocaml/opam:ubuntu-ocaml-" ^ base_image_version)
