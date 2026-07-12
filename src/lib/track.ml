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

module Track = struct
  type t = No_context

  (* v2: [Value] now embeds the commit the packages were read from
     (see the glitch note on {!v}). The id bump cleanly separates the
     old cached outcomes, whose JSON no longer unmarshals. *)
  let id = "opam-repo-track-v2"
  let auto_cancel = true

  module Key = struct
    type t = { limit : int option; repo : Git.Commit.t; filter : string list }

    let digest { repo; filter; limit } =
      Git.Commit.hash repo
      ^ String.concat ";" filter
      ^ "; "
      ^ (limit |> Option.map string_of_int |> Option.value ~default:"")
  end

  let pp f { Key.repo; filter; limit } =
    let limit_s = match limit with
      | None -> "all"
      | Some n -> string_of_int n
    in
    Fmt.pf f "opam repo track (limit=%s) %a [%a]"
      limit_s Git.Commit.pp_short repo
      Fmt.(list ~sep:(any ",") string) filter

  module Value = struct
    type package_definition = { package : OpamPackage.t; digest : string }
    [@@deriving yojson]

    (* The commit rides in the value so consumers can check that a
       (possibly latched) tracking result actually corresponds to the
       repo state the rest of their inputs were derived from. *)
    type t = {
      commit : string;
      packages : package_definition list;
    }
    [@@deriving yojson]

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
           v
           |> List.sort (fun a b ->
                  -OpamPackage.compare a.Value.package b.package)
           |> take limit)

  let build No_context job { Key.repo; filter; limit } =
    let open Lwt.Syntax in
    let open Rresult in
    let filter name =
      match filter with [] -> true | lst -> List.mem (Fpath.basename name) lst
    in
    Log.info (fun f ->
        f "Tracking packages in %a" Fpath.pp (Git.Commit.repo repo));
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job repo @@ fun dir ->
    let result =
      Bos.OS.Dir.contents Fpath.(dir / "packages") >>| fun packages ->
      packages
      (* Skip non-directory entries. Some repos keep a README.md
         directly under [packages/] (e.g. local overlays), which
         isn't a package and would crash [get_versions] when it
         tries to list versions beneath it. *)
      |> List.filter (fun p ->
           match Bos.OS.Dir.exists p with Ok true -> true | _ -> false)
      |> List.filter filter
      |> Lwt_list.map_s (get_versions ~limit)
      |> Lwt.map (fun v ->
           Value.{ commit = Git.Commit.hash repo;
                   packages = List.flatten v })
    in
    match result with
    | Ok v -> Lwt.map Result.ok v
    | Error e -> Lwt.return_error e
end

module LatchedBuilder (B : Current_cache.S.BUILDER) = struct
  module Adaptor = struct
    type t = B.t
    let id = B.id
    module Key = Current.String
    module Value = B.Key
    module Outcome = B.Value
    let run op job _ key = B.build op job key
    let pp f (_, key) = B.pp f key
    let auto_cancel = B.auto_cancel
    let latched = true
  end
  include Current_cache.Generic (Adaptor)
  let get ~opkey ?schedule ctx key = run ?schedule ctx opkey key
end

module TrackCache = LatchedBuilder (Track)
open Track.Value

type t = package_definition [@@deriving yojson]

let pkg t = t.package
let digest t = t.digest

module Map = OpamStd.Map.Make (struct
  type nonrec t = t

  let compare a b = OpamPackage.compare a.package b.package

  let to_json { package; digest } =
    `A [ OpamPackage.to_json package; `String digest ]

  let of_json _ = None
  let to_string t = OpamPackage.to_string t.package
end)

let v ~repo_label ~limit ~(filter : string list)
    (repo : Git.Commit.t Current.t) =
  let open Current.Syntax in
  (* [repo_label] distinguishes same-(filter, limit) calls that feed
     from different repos — e.g. the ocaml mainline + oxcaml overlay
     fan-out in a single profile, or two profiles that both track
     mainline. Without it, OCurrent treats the shared component as
     one "instance" and errors "set to different values in the same
     step" when the input commits don't match.

     The result pairs the package list with the commit it was read
     from. The op is {e latched}: right after the input commit moves,
     the current still reports the {e previous} commit's packages
     while the re-track runs. Consumers combining this with other
     repo-derived inputs (the solver) must check the embedded commit
     against their view of the repo and skip mismatched evaluations —
     see {!Docs_ci_lib.Day11_solver.solve}. *)
  let limit_s = match limit with
    | None -> "all"
    | Some n -> string_of_int n
  in
  let reduced_filter =
    if List.length filter <= 3 then filter else
      List.take 3 filter @ ["..."]
  in
  Current.component "Track %s (limit=%s) - %a" repo_label limit_s
    Fmt.(list string) reduced_filter
  |> let> repo in
     (* opkey disambiguates at the LatchedBuilder layer too. *)
     let opkey = Printf.sprintf "track-%s-%s-%s"
       repo_label limit_s (String.concat "," filter) in
     TrackCache.get ~opkey No_context { filter; repo; limit }
     |> Current.Primitive.map_result
          (Result.map (fun (v : Track.Value.t) -> (v.commit, v.packages)))

(** Union per-repo tracking results (as plain values), with later
    repos' entries overriding earlier by [(name, version)] —
    mirroring opam's overlay resolution. Sorted by package for a
    deterministic order (it feeds cache-key digests). *)
let merge_values (per_repo : t list list) : t list =
  let table = Hashtbl.create 1024 in
  List.iter (fun pkgs ->
    List.iter (fun (pkg : t) ->
      Hashtbl.replace table pkg.package pkg
    ) pkgs
  ) per_repo;
  Hashtbl.fold (fun _ v acc -> v :: acc) table []
  |> List.sort (fun (a : t) b -> OpamPackage.compare a.package b.package)
  |> List.sort (fun a b -> -(OpamPackage.compare a.package b.package))
