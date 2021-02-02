type t = OpamParserTypes.opamfile

type pkg = { name : string; version : string; repo : string }

let get_packages (opam_file : t) =
  let open OpamParserTypes in
  let pin_depends =
    List.find_map
      (function Variable (_, name, value) when name = "pin-depends" -> Some value | _ -> None)
      opam_file.file_contents
    |> Option.get
    (* what if no package ? *)
  in
  List.filter_map
    (function
      | List (_, String (_, name_version) :: String (_, repo) :: _) ->
          let name, version =
            match String.split_on_char '.' name_version with
            | name :: version -> (name, String.concat "." version)
            | _ -> failwith "no version"
          in
          Some { name; version; repo }
      | _ -> None)
    (match pin_depends with List (_, v) -> v | _ -> failwith "failed to parse opam")

let marshal = OpamPrinter.opamfile

let unmarshal t = OpamParser.string t "monorepo.opam"

let digest = marshal

let to_yojson f = `String (OpamPrinter.opamfile f)

let of_yojson = function
  | `String s -> Ok (OpamParser.string s "")
  | _ -> Error "failed to parse opamfile"
