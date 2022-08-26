module rec Universe : sig
  type t [@@deriving yojson]

  val hash : t -> string
  val deps : t -> Package.t list
  val pp : t Fmt.t
  val v : Package.t list -> t
  val compare : t -> t -> int
end = struct
  type t = { hash : string; deps : Package.t list } [@@deriving yojson]

  let hash t = t.hash
  let deps t = t.deps

  let v deps =
    let str =
      deps |> List.map Package.opam |> List.sort OpamPackage.compare
      |> List.fold_left (fun acc p -> Format.asprintf "%s\n%s" acc (OpamPackage.to_string p)) ""
    in
    let hash = Digest.to_hex (Digest.string str) in
    { hash; deps }

  let pp f { hash; _ } = Fmt.pf f "%s" hash
  let compare { hash; _ } { hash = hash2; _ } = String.compare hash hash2
end

and Package : sig
  type t [@@deriving yojson]

  val opam : t -> OpamPackage.t
  val commit : t -> string
  val universe : t -> Universe.t
  val digest : t -> string
  val id : t -> string
  val pp : t Fmt.t
  val compare : t -> t -> int
  val v : OpamPackage.t -> t list -> string -> t

  val make :
    blacklist:string list ->
    commit:string ->
    root:OpamPackage.t ->
    (OpamPackage.t * OpamPackage.t list) list ->
    t
end = struct
  type t = { opam : O.OpamPackage.t; universe : Universe.t; commit : string } [@@deriving yojson]

  let universe t = t.universe
  let opam t = t.opam
  let commit t = t.commit
  let id t = OpamPackage.to_string t.opam ^ "-" ^ Universe.hash t.universe
  let digest = id
  let v opam deps commit = { opam; universe = Universe.v deps; commit }

  let pp f { universe; opam; _ } =
    Fmt.pf f "%s; %a" (OpamPackage.to_string opam) Universe.pp universe

  let compare t t2 =
    match OpamPackage.compare t.opam t2.opam with
    | 0 -> Universe.compare t.universe t2.universe
    | v -> v

  let remove_blacklisted_packages ~blacklist deps =
    let module StringSet = Set.Make (String) in
    let blacklist = StringSet.of_list blacklist in
    let filter pkg = not (StringSet.mem (OpamPackage.name_to_string pkg) blacklist) in
    deps
    |> List.filter (fun (pkg, _) -> filter pkg)
    |> List.map (fun (pkg, deps) -> (pkg, List.filter filter deps))

  let make ~blacklist ~commit ~root deps =
    let deps = remove_blacklisted_packages ~blacklist deps in
    let memo = ref OpamPackage.Map.empty in
    let package_deps = OpamPackage.Map.of_list deps in
    let rec obtain package =
      match OpamPackage.Map.find_opt package !memo with
      | Some package -> package
      | None ->
          memo := OpamPackage.Map.add package None !memo;
          let deps_pkg =
            OpamPackage.Map.find_opt package package_deps
            |> Option.value ~default:[] |> List.filter_map obtain
          in
          let pkg = Some (Package.v package deps_pkg commit) in
          memo := OpamPackage.Map.add package pkg !memo;
          pkg
    in
    obtain root |> Option.get
end

include Package

let all_deps pkg = pkg :: (pkg |> universe |> Universe.deps)

module PackageMap = Map.Make (Package)
module PackageSet = Set.Make (Package)

module Blessing = struct
  type t = Blessed | Universe

  let is_blessed t = t = Blessed
  let of_bool t = if t then Blessed else Universe
  let to_string = function Blessed -> "blessed" | Universe -> "universe"

  module Set = struct
    type b = t

    module StringSet = Set.Make (String)

    type t = { opam : OpamPackage.t; universe : string; blessed: Package.t option }

    let universe_size u = Universe.deps u |> List.length
    let empty (opam : OpamPackage.t) : t = { opam; universe = ""; blessed = None }

    module Universe_info = struct
      type t = { universe : Universe.t; deps_count : int; revdeps_count : int }

      (* To compare two possibilities, we want first to maximize the number of dependencies
         in the universe (to favorize optional dependencies) and then maximize the number of revdeps:
         this is for stability purposes, as any blessing change will force downstream recomputations. *)
      let compare a b =
        match Int.compare a.deps_count b.deps_count with
        | 0 -> Int.compare a.revdeps_count b.revdeps_count
        | v -> v

      let make ~counts package =
        let universe = Package.universe package in
        let deps_count = universe_size universe in
        { universe; deps_count; revdeps_count = PackageMap.find package counts }
    end

    let v ~counts (packages : Package.t list) : t =
      assert (packages <> []);
      let first_package = List.hd packages in
      let opam = first_package |> Package.opam in
      let first_universe = Universe_info.make ~counts first_package in
      let best_package, best_universe =
        List.fold_left
          (fun (best_package, best_universe) new_package ->
            assert (Package.opam new_package = opam);
            let new_universe = Universe_info.make ~counts new_package in
            if Universe_info.compare new_universe best_universe > 0 then new_package, new_universe
            else (best_package, best_universe))
          (first_package, first_universe) (List.tl packages)
      in
      { opam; 
        universe = Universe.hash best_universe.universe; 
        blessed = Some best_package }

    let get { opam; universe; _ } pkg =
      assert (Package.opam pkg = opam);
      of_bool (Universe.hash (Package.universe pkg) = universe)
    
    let blessed t = 
      match t.blessed with
      | None -> failwith "Blessed package set is empty"
      | Some v -> v
  end
end

module Map = PackageMap
module Set = PackageSet
