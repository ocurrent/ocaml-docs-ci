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
    type t = { limit : int option; repo : Git.Commit.t; filter : string list }

    let digest { repo; filter; limit } =
      Git.Commit.hash repo ^ String.concat ";" filter ^ "; "
      ^ (limit |> Option.map string_of_int |> Option.value ~default:"")
  end

  let pp f { Key.repo; filter; _ } =
    Fmt.pf f "opam repo track\n%a\n%a" Git.Commit.pp_short repo Fmt.(list string) filter

  module Value = struct
    type package_definition = { package : OpamPackage.t; digest : string } [@@deriving yojson]

    type t = package_definition list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let rec take n lst =
    match (n, lst) with 0, _ -> [] | _, [] -> [] | n, a :: q -> a :: take (n - 1) q

  let take = function Some n -> take n | None -> Fun.id

  let get_file path = Lwt_io.with_file ~mode:Input (Fpath.to_string path) Lwt_io.read

  let get_versions ~limit path =
    let open Lwt.Syntax in
    let open Rresult in
    Bos.OS.Dir.contents path
    >>| (fun versions ->
          versions
          |> Lwt_list.map_p (fun path ->
                 let+ content = get_file Fpath.(path / "opam") in
                 Value.
                   {
                     package = path |> Fpath.basename |> OpamPackage.of_string;
                     digest = Digest.(string content |> to_hex);
                   }))
    |> Result.get_ok
    |> Lwt.map (fun v ->
           v |> List.sort (fun a b -> -OpamPackage.compare a.Value.package b.package) |> take limit)

  let build No_context job { Key.repo; filter; limit } =
    let open Lwt.Syntax in
    let open Rresult in
    let filter name = match filter with [] -> true | lst -> List.mem (Fpath.basename name) lst in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job repo @@ fun dir ->
    let result =
      Bos.OS.Dir.contents Fpath.(dir / "packages") >>| fun packages ->
      packages |> List.filter filter
      |> Lwt_list.map_s (get_versions ~limit)
      |> Lwt.map (fun v -> List.flatten v)
    in
    match result with Ok v -> Lwt.map Result.ok v | Error e -> Lwt.return_error e
end

module TrackCache = Misc.LatchedBuilder (Track)
open Track.Value

type t = package_definition [@@deriving yojson]

let pkg t = t.package

let digest t = t.digest

module Map = OpamStd.Map.Make (struct
  type nonrec t = t

  let compare a b = O.OpamPackage.compare a.package b.package

  let to_json { package; digest } = `A [ OpamPackage.to_json package; `String digest ]

  let of_json _ = None

  let to_string t = OpamPackage.to_string t.package
end)

let v ~limit ~(filter : string list) (repo : Git.Commit.t Current.t) =
  let open Current.Syntax in
  Current.component "Track packages - %a" Fmt.(list string) filter
  |> let> repo = repo in
     (* opkey is a constant because we expect only one instance of track *)
     TrackCache.get ~opkey:"track" No_context { filter; repo; limit }
