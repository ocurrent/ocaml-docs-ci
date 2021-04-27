module Git = Current_git

module OpamPackage = struct
  include OpamPackage

  let to_yojson t = `String (OpamPackage.to_string t)

  let of_yojson = function
    | `String str -> (
        match OpamPackage.of_string_opt str with
        | Some x -> Ok x
        | None -> Error "failed to parse version" )
    | _ -> Error "failed to parse version"
end

module Track = struct
  type t = No_context

  let id = "opam-repo-track"

  let auto_cancel = true

  module Key = struct
    type t = { repo : Git.Commit.t; filter : string list }

    let digest { repo; filter } = Git.Commit.hash repo ^ String.concat ";" filter
  end

  let pp f { Key.repo; filter } =
    Fmt.pf f "opam repo track\n%a\n%a" Git.Commit.pp_short repo Fmt.(list string) filter

  module Value = struct
    type t = (OpamPackage.t * string) list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let rec take n lst =
    match (n, lst) with 0, _ -> [] | _, [] -> [] | n, a :: q -> a :: take (n - 1) q

  let take = match Config.take_n_last_versions with 
    | Some n -> take n
    | None -> Fun.id

  let get_digest path =
    let content = Bos.OS.File.read path |> Result.get_ok in
    Digestif.SHA256.(digest_string content |> to_hex)

  let get_versions path =
    let open Rresult in
    Bos.OS.Dir.contents path
    >>| (fun versions ->
          versions
          |> List.rev_map (fun path ->
                 (path |> Fpath.basename |> OpamPackage.of_string, get_digest Fpath.(path / "opam"))))
    |> Result.get_ok
    |> List.sort (fun (a, _) (b, _) -> OpamPackage.compare a b)
    |> List.rev |> take

  let build No_context job { Key.repo; filter } =
    let open Lwt.Syntax in
    let open Rresult in
    let filter name = match filter with [] -> true | lst -> List.mem (Fpath.basename name) lst in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job repo @@ fun dir ->
    Bos.OS.Dir.contents Fpath.(dir / "packages")
    >>= (fun packages ->
          packages |> List.filter filter |> List.rev_map get_versions |> List.flatten |> Result.ok)
    |> Lwt.return
end

module TrackCache = Current_cache.Make (Track)

type t = O.OpamPackage.t * string [@@deriving yojson]

let digest = snd

let pkg = fst

module Map = OpamStd.Map.Make (struct
  type nonrec t = t

  let compare (a, _) (b, _) = O.OpamPackage.compare a b

  let to_json (pkg, digest) = `A [ OpamPackage.to_json pkg; `String digest ]

  let of_json _ = None

  let to_string (pkg, _) = OpamPackage.to_string pkg
end)

let v ~(filter : string list) (repo : Git.Commit.t Current.t) =
  let open Current.Syntax in
  Current.component "Track packages - %a" Fmt.(list string) filter
  |> let> repo = repo in
     TrackCache.get No_context { filter; repo }
