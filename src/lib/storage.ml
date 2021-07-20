module Base = struct
  type repository =
    | HtmlTailwind of Epoch.t
    | HtmlClassic of Epoch.t
    | Linked of Epoch.t
    | Compile
    | Prep

  let generation_folder stage generation = Fpath.(v ("epoch-" ^ Epoch.digest stage generation))

  let folder = function
    | HtmlTailwind generation -> Fpath.(generation_folder `Html generation / "html-tailwind")
    | HtmlClassic generation -> Fpath.(generation_folder `Html generation / "html-classic")
    | Linked generation -> Fpath.(generation_folder `Linked generation / "linked")
    | Compile -> Fpath.v "compile"
    | Prep -> Fpath.v "prep"
end

type repository =
  | HtmlTailwind of (Epoch.t * Package.Blessing.t)
  | HtmlClassic of (Epoch.t * Package.Blessing.t)
  | Linked of (Epoch.t * Package.Blessing.t)
  | Compile of Package.Blessing.t
  | Prep

let to_base_repo = function
  | HtmlClassic (t, _) -> Base.HtmlClassic t
  | HtmlTailwind (t, _) -> Base.HtmlTailwind t
  | Linked (t, _) -> Linked t
  | Compile _ -> Compile
  | Prep -> Prep

let base_folder ~blessed package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fpath.(v "packages" / name / version)
  else Fpath.(v "universes" / universe / name / version)

let folder repository package =
  let blessed =
    match repository with
    | HtmlTailwind (_, b) | HtmlClassic (_, b) | Linked (_, b) | Compile b -> b
    | Prep -> Universe
  in
  let blessed = blessed = Blessed in
  Fpath.(Base.folder (to_base_repo repository) // base_folder ~blessed package)

let for_all packages command =
  let data =
    let pp_package f (repository, package) =
      let dir = folder repository package |> Fpath.to_string in
      let id = Package.id package in
      Fmt.pf f "%s,%s,%s" dir id (Package.opam package |> OpamPackage.to_string)
    in
    Fmt.(to_to_string (list ~sep:(const string " ") pp_package) packages)
  in
  Fmt.str "for DATA in %s; do IFS=\",\"; set -- $DATA; %s done" data command

type id_hash = { id : string; hash : string } [@@deriving yojson]

module Tar = struct
  let hash_command ~prefix =
    Fmt.str
      "HASH=$((sha256sum $1/content.tar | cut -d \" \" -f 1)  || echo -n 'empty'); printf \
       \"%s:$2:$HASH\\n\";"
      prefix
end

let hash_command ~prefix =
  Fmt.str
    "HASH=$(find $1 -type f -exec sha256sum {} \\; | sort | sha256sum); printf \"%s:$2:$HASH\\n\";"
    prefix

let parse_hash ~prefix line =
  match String.split_on_char ':' line with
  | [ prev; id; hash ] when Astring.String.is_suffix ~affix:prefix prev -> Some { id; hash }
  | _ -> None
