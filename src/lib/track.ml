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

  (* v2: [Value] embeds the commit the packages were read from (see
     the glitch note on {!v}).
     v3: per-package digests are version-directory tree OIDs read
     straight from the git store (covering opam + files/ patches),
     replacing md5-of-opam-content over a full checkout — one tree
     read per package name instead of materialising and hashing ~38k
     files. Each id bump cleanly separates older cached outcomes. *)
  let id = "opam-repo-track-v3"
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
    let limit_s =
      match limit with None -> "all" | Some n -> string_of_int n
    in
    Fmt.pf f "opam repo track (limit=%s) %a [%a]" limit_s Git.Commit.pp_short
      repo
      Fmt.(list ~sep:(any ",") string)
      filter

  module Value = struct
    type package_definition = { package : OpamPackage.t; digest : string }
    [@@deriving yojson]

    (* The commit rides in the value so consumers can check that a
       (possibly latched) tracking result actually corresponds to the
       repo state the rest of their inputs were derived from. *)
    type t = { commit : string; packages : package_definition list }
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

  let build No_context job { Key.repo; filter; limit } =
    let open Lwt.Syntax in
    let filter name =
      match filter with [] -> true | lst -> List.mem name lst
    in
    Log.info (fun f ->
        f "Tracking packages in %a" Fpath.pp (Git.Commit.repo repo));
    let* () = Current.Job.start ~level:Harmless job in
    (* Read the package list straight from the git store at the
       commit — no checkout, no file content: each package's
       fingerprint is its version-directory tree OID. *)
    Lwt.catch
      (fun () ->
        let path = Fpath.to_string (Git.Commit.repo repo) in
        let hash = Git.Commit.hash repo in
        let* store, commit =
          Day11_opam.Git_utils.get_git_repo_store_and_hash_commit_lwt path
            (Some hash)
        in
        let* entries =
          Day11_opam.Git_packages.list_package_versions_lwt ~store commit
        in
        let packages =
          entries
          |> List.filter (fun (pkg, _) ->
                 filter (OpamPackage.Name.to_string (OpamPackage.name pkg)))
          |> List.map (fun (pkg, oid) -> Value.{ package = pkg; digest = oid })
          (* Group by name to apply [limit] (newest N versions per
             name), matching the historical per-name semantics. *)
          |> List.fold_left
               (fun m (e : Value.package_definition) ->
                 let n = OpamPackage.name e.package in
                 OpamPackage.Name.Map.update n (fun es -> e :: es) [] m)
               OpamPackage.Name.Map.empty
          |> OpamPackage.Name.Map.values
          |> List.concat_map (fun es ->
                 es
                 |> List.sort (fun (a : Value.package_definition) b ->
                        -OpamPackage.compare a.package b.package)
                 |> take limit)
        in
        Lwt.return_ok Value.{ commit = Git.Commit.hash repo; packages })
      (fun exn ->
        Lwt.return_error
          (`Msg (Printf.sprintf "track failed: %s" (Printexc.to_string exn))))
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

let v ~repo_label ~limit ~(filter : string list) (repo : Git.Commit.t Current.t)
    =
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
  let limit_s = match limit with None -> "all" | Some n -> string_of_int n in
  let reduced_filter =
    if List.length filter <= 3 then filter else List.take 3 filter @ [ "..." ]
  in
  Current.component "Track %s (limit=%s) - %a" repo_label limit_s
    Fmt.(list string)
    reduced_filter
  |> let> repo in
     (* opkey disambiguates at the LatchedBuilder layer too. *)
     let opkey =
       Printf.sprintf "track-%s-%s-%s" repo_label limit_s
         (String.concat "," filter)
     in
     TrackCache.get ~opkey No_context { filter; repo; limit }
     |> Current.Primitive.map_result
          (Result.map (fun (v : Track.Value.t) -> (v.commit, v.packages)))

(** Union per-repo tracking results (as plain values), with later repos' entries
    overriding earlier by [(name, version)] — mirroring opam's overlay
    resolution. Sorted by package for a deterministic order (it feeds cache-key
    digests). *)
let merge_values (per_repo : t list list) : t list =
  let table = Hashtbl.create 1024 in
  List.iter
    (fun pkgs ->
      List.iter (fun (pkg : t) -> Hashtbl.replace table pkg.package pkg) pkgs)
    per_repo;
  Hashtbl.fold (fun _ v acc -> v :: acc) table []
  |> List.sort (fun (a : t) b -> OpamPackage.compare a.package b.package)
  |> List.sort (fun a b -> -OpamPackage.compare a.package b.package)
