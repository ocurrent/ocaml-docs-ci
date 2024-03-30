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
      deps
      |> List.map Package.opam
      |> List.sort OpamPackage.compare
      |> List.fold_left
           (fun acc p -> Format.asprintf "%s\n%s" acc (OpamPackage.to_string p))
           ""
    in
    let hash = Digest.to_hex (Digest.string str) in
    { hash; deps }

  let pp f { hash; _ } = Fmt.pf f "%s" hash
  let compare { hash; _ } { hash = hash2; _ } = String.compare hash hash2
end

and Package : sig
  type t [@@deriving yojson]

  val group : t -> t list option
  val opam : t -> OpamPackage.t
  val commit : t -> string
  val universe : t -> Universe.t
  val universes : t -> Universe.t list
  val digest : t -> string
  val universes_hash : t -> string
  val id : t -> string
  val pp : t Fmt.t
  val compare : t -> t -> int
  val v : ?group:Package.t list -> OpamPackage.t -> t list -> string -> t

  val make :
    ?group:OpamPackage.t list ->
    blacklist:string list ->
    commit:string ->
    root:OpamPackage.t ->
    (OpamPackage.t * OpamPackage.t list) list ->
    t
end = struct
  type t = {
    group : Package.t list option;
    opam : O.OpamPackage.t;
    universe : Universe.t;
    commit : string;
  }
  [@@deriving yojson]

  let group t = t.group
  let universe t = t.universe

  let universes t =
    Option.value ~default:[ universe t ]
    @@ Option.map (fun pkg -> List.map universe pkg) (group t)

  let universes_hash t =
    match t.group with
    | None -> universe t |> Universe.hash
    | Some group ->
        let hashes =
          List.map (fun pkg -> universe pkg |> Universe.hash) group
        in
        String.concat "" hashes |> Digest.string |> Digest.to_hex

  let opam t = t.opam
  let commit t = t.commit
  let id t = OpamPackage.to_string t.opam ^ "-" ^ universes_hash t
  let digest = id

  let universes_hash t =
    let univs =
      (t.universe |> Universe.hash)
      :: List.map
           (fun pkg -> universe pkg |> Universe.hash)
           (Option.to_list t.group |> List.flatten)
    in
    String.concat "" univs |> Digest.string |> Digest.to_hex

  let v ?group opam deps commit =
    { group; opam; universe = Universe.v deps; commit }

  let pp f { universe; opam; _ } =
    Fmt.pf f "%s; %a" (OpamPackage.to_string opam) Universe.pp universe

  let compare t t2 =
    match OpamPackage.compare t.opam t2.opam with
    | 0 -> Universe.compare t.universe t2.universe
    | v -> v

  let remove_blacklisted_packages ~blacklist deps =
    let module StringSet = Set.Make (String) in
    let blacklist = StringSet.of_list blacklist in
    let filter pkg =
      not (StringSet.mem (OpamPackage.name_to_string pkg) blacklist)
    in
    deps
    |> List.filter (fun (pkg, _) -> filter pkg)
    |> List.map (fun (pkg, deps) -> (pkg, List.filter filter deps))

  let make ?group ~blacklist ~commit ~root deps =
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
            |> Option.value ~default:[]
            |> List.filter_map obtain
          in
          let pkg = Some (Package.v package deps_pkg commit) in
          memo := OpamPackage.Map.add package pkg !memo;
          pkg
    in
    let group =
      group
      |> Option.map (fun group ->
             List.map (fun pkg -> obtain pkg |> Option.get) group)
    in
    obtain root |> Option.map (fun t -> { t with group }) |> Option.get
end

include Package

let all_deps pkg =
  let deps pkg = pkg :: (pkg |> universe |> Universe.deps) in
  match Package.group pkg with
  | None -> deps pkg
  | Some group ->
      (* The root pkg is also in the group *)
      List.map (fun pkg -> deps pkg) group |> List.flatten

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

    type t = {
      opam : OpamPackage.t;
      universe : string;
      blessed : Package.t option;
    }

    let universes_size u =
      List.map Universe.deps u |> List.flatten |> List.length

    let empty (opam : OpamPackage.t) : t =
      { opam; universe = ""; blessed = None }

    module Universe_info = struct
      type t = { pkg : Package.t; deps_count : int; revdeps_count : int }

      (* To compare two possibilities, we want first to maximize the number of dependencies
         in the universe (to favorize optional dependencies) and then maximize the number of revdeps:
         this is for stability purposes, as any blessing change will force downstream recomputations. *)
      let compare a b =
        match Int.compare a.deps_count b.deps_count with
        | 0 -> Int.compare a.revdeps_count b.revdeps_count
        | v -> v

      let make ~counts package =
        let universes = Package.universes package in
        let deps_count = universes_size universes in
        {
          pkg = package;
          deps_count;
          revdeps_count = PackageMap.find package counts;
        }
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
            if Universe_info.compare new_universe best_universe > 0 then
              (new_package, new_universe)
            else (best_package, best_universe))
          (first_package, first_universe)
          (List.tl packages)
      in
      {
        opam;
        universe = Package.universes_hash best_universe.pkg;
        blessed = Some best_package;
      }

    let get { opam; universe; _ } pkg =
      assert (Package.opam pkg = opam);
      of_bool (Universe.hash (Package.universe pkg) = universe)

    let blessed t = t.blessed
  end
end

module Map = PackageMap
module Set = PackageSet
