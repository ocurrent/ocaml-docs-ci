module Git = Current_git

module OpamPackage = struct
  include OpamPackage

  let to_yojson t = `String (OpamPackage.to_string t)

  let of_yojson = function
    | `String str -> (
        match OpamPackage.of_string_opt str with
        | Some x -> Ok x
        | None -> Error "failed to parse version")
    | _ -> Error "failed to parse version"
end

module Url_repo = struct
  include OpamUrl

  let comparable_repo t =
    t.path
    |> Astring.String.span ~rev:true ~min:0 ~max:4
    |> (fun (p, git) -> if git = ".git" then p else t.path)
    |> (fun path ->
         Astring.String.span ~min:0 ~max:4 path |> fun (git, p) ->
         if git = "git@" then p else path)
    |> Astring.String.map (fun c -> if c = ':' then '/' else c)
end

module Track = struct
  type t = No_context

  let id = "opam-repo-track"
  let auto_cancel = true

  module Key = struct
    type t = {
      limit : int option;
      repo : Git.Commit.t;
      filter : string list;
      group : bool;
    }

    let digest { repo; filter; limit; group } =
      Git.Commit.hash repo
      ^ String.concat ";" filter
      ^
      if group then "; "
      else
        "(group); "
        ^ (limit |> Option.map string_of_int |> Option.value ~default:"")
  end

  let pp f { Key.repo; filter; _ } =
    Fmt.pf f "opam repo track\n%a\n%a" Git.Commit.pp_short repo
      Fmt.(list string)
      filter

  module Value = struct
    type group_packages = { packages : OpamPackage.t list; digest : string }
    [@@deriving yojson]

    type t = group_packages list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let rec take n lst =
    match (n, lst) with
    | 0, _ -> []
    | _, [] -> []
    | n, a :: q -> a :: take (n - 1) q

  let take = function Some n -> take n | None -> Fun.id

  let get_file path =
    Lwt_io.with_file ~mode:Input (Fpath.to_string path) Lwt_io.read

  let get_versions ~group ~limit path =
    let open Lwt.Syntax in
    let open Rresult in
    Bos.OS.Dir.contents path
    >>| (fun versions ->
          versions
          |> Lwt_list.map_p (fun path ->
                 let+ content = get_file Fpath.(path / "opam") in
                 let package =
                   path |> Fpath.basename |> OpamPackage.of_string
                 in
                 if not group then
                   ((None, Digest.(string content |> to_hex)), package)
                 else
                   OpamFile.OPAM.read_from_string content |> fun opam ->
                   ( ( OpamFile.OPAM.dev_repo opam
                       |> Option.map Url_repo.comparable_repo,
                       Digest.(string content |> to_hex) ),
                     package )))
    |> Result.get_ok
    |> Lwt.map (fun v ->
           v
           |> List.sort (fun a b -> -OpamPackage.compare (snd a) (snd b))
           |> take limit)

  let digest_concat digest =
    match digest with
    | d :: [] -> d
    | group -> Astring.String.concat group |> Digest.string |> Digest.to_hex

  let build No_context job { Key.repo; filter; limit; group } =
    let open Lwt.Syntax in
    let open Rresult in
    let open Lwt.Infix in
    let filter name =
      match filter with [] -> true | lst -> List.mem (Fpath.basename name) lst
    in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job repo @@ fun dir ->
    let repo_version_group = Hashtbl.create 10 in
    let result =
      Bos.OS.Dir.contents Fpath.(dir / "packages") >>| fun packages ->
      packages
      |> List.filter filter
      |> Lwt_list.map_s (get_versions ~group ~limit)
      >>= fun v ->
      List.flatten v |> fun v ->
      Current.Job.log job "Tracked packages: %d" (List.length v);
      v
      |> Lwt_list.iter_s (fun ((dev_repo, digest), pkg) ->
             let key =
               match dev_repo with
               | None -> OpamPackage.to_string pkg
               | Some repo ->
                   OpamPackage.version pkg |> OpamPackage.Version.to_string
                   |> fun version ->
                   repo ^ version |> Digest.string |> Digest.to_hex
               (* The need to group the packages by their dev-repo and version *)
             in
             match Hashtbl.find_opt repo_version_group key with
             | Some group ->
                 Lwt.return
                 @@ Hashtbl.replace repo_version_group key
                      ((pkg, digest) :: group)
             | None ->
                 Lwt.return
                 @@ Hashtbl.replace repo_version_group key [ (pkg, digest) ])
      |> Lwt.map (fun () ->
             Current.Job.log job "Tracked jobs: %d"
             @@ Hashtbl.length repo_version_group;
             Hashtbl.to_seq repo_version_group
             |> Seq.map (fun (_, group) ->
                    let group = List.rev group in
                    {
                      Value.packages = List.map fst group;
                      digest = digest_concat @@ List.map snd group;
                    })
             |> List.of_seq)
    in
    match result with
    | Ok v -> Lwt.map Result.ok v
    | Error e -> Lwt.return_error e
end

module TrackCache = Misc.LatchedBuilder (Track)
open Track.Value

type t = group_packages [@@deriving yojson]

let pkgs t = t.packages
let digest t = t.digest

let v ?(group = false) ~limit ~(filter : string list)
    (repo : Git.Commit.t Current.t) =
  let open Current.Syntax in
  Current.component "Track packages - %a" Fmt.(list string) filter
  |> let> repo in
     if not group then
       TrackCache.get ~opkey:"track" No_context { filter; repo; limit; group }
     else
       TrackCache.get ~opkey:"track-(group)" No_context
         { filter; repo; limit; group }
