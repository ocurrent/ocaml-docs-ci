type phase = Compile | Link | Doc_all

let string_of_phase = function
  | Compile -> "compile"
  | Link -> "link"
  | Doc_all -> "doc-all"

let phase_of_string = function
  | "compile" -> Compile
  | "link" -> Link
  | "doc-all" -> Doc_all
  | _ -> Doc_all

(* Internal wire record so we can use ppx_deriving_yojson while
   keeping [phase] as a typed variant in the public type. *)
type wire = {
  package : string;
  phase : string;
  deps : string list; [@default []]
}
[@@deriving yojson { strict = false }]

type t = { package : string; phase : phase; deps : string list }

let to_wire (t : t) : wire =
  { package = t.package; phase = string_of_phase t.phase; deps = t.deps }

let of_wire (w : wire) : t =
  { package = w.package; phase = phase_of_string w.phase; deps = w.deps }

let filename = "doc.json"

let save layer_dir t =
  let path = Fpath.(layer_dir / "doc.json") in
  try
    Yojson.Safe.to_file (Fpath.to_string path) (wire_to_yojson (to_wire t));
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Doc_meta.save %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let load layer_dir =
  let path = Fpath.(layer_dir / "doc.json") in
  try
    match wire_of_yojson (Yojson.Safe.from_file (Fpath.to_string path)) with
    | Ok w -> Ok (of_wire w)
    | Error msg -> Rresult.R.error_msgf "Doc_meta.load %a: %s" Fpath.pp path msg
  with exn ->
    Rresult.R.error_msgf "Doc_meta.load %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let exists layer_dir =
  Bos.OS.File.exists Fpath.(layer_dir / "doc.json")
  |> Result.value ~default:false
