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
    type t = OpamPackage.t list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job { Key.repo; filter } =
    let open Lwt.Syntax in
    let open Rresult in
    let filter name = match filter with [] -> true | lst -> List.mem (Fpath.basename name) lst in
    let get_versions path =
      Bos.OS.Dir.contents path
      >>| (fun versions ->
            versions |> List.rev_map (fun path -> path |> Fpath.basename |> OpamPackage.of_string))
      |> Result.get_ok
      |> function (* take 3 versions *)
      | a::b::c::_ -> [a; b; c]
      | r -> r
    in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job repo @@ fun dir ->
    Bos.OS.Dir.contents Fpath.(dir / "packages")
    >>= (fun packages ->
          packages |> List.filter filter |> List.rev_map get_versions |> List.flatten |> Result.ok)
    |> Lwt.return
end

module TrackCache = Current_cache.Make (Track)

let track_packages ~(filter : string list) (repo : Git.Commit.t Current.t) =
  let open Current.Syntax in
  Current.component "Track packages - %a" Fmt.(list string) filter
  |> let> repo = repo in
     TrackCache.get No_context { filter; repo }
