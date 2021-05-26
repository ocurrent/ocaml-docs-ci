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

  let digest t = OpamPackage.to_string t.opam ^ "-" ^ Universe.hash t.universe

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

and Blessed : sig
  type t

  val v : Package.t list -> t

  val is_blessed : t -> Package.t -> bool
end = struct
  module StringSet = Set.Make (String)

  type t = { opam : OpamPackage.t; universe : string }

  let universe_size u = Universe.deps u |> List.length

  let v (packages : Package.t list) : t =
    let first_package = List.hd packages in
    let opam = first_package |> Package.opam in
    let first_universe = first_package |> Package.universe in
    let best_universe, _ =
      List.fold_left
        (fun (cur_best_universe, cur_best_universe_size) new_package ->
          assert (Package.opam new_package = opam);
          let new_universe = Package.universe new_package in
          let size = universe_size new_universe in
          if size > cur_best_universe_size then (new_universe, size)
          else (cur_best_universe, cur_best_universe_size))
        (first_universe, universe_size first_universe)
        (List.tl packages)
    in
    { opam; universe = Universe.hash best_universe }

  let is_blessed { opam; universe } pkg =
    assert (Package.opam pkg = opam);
    Universe.hash (Package.universe pkg) = universe
end

include Package

let all_deps pkg = pkg :: (pkg |> universe |> Universe.deps)

module Map = Map.Make (Package)
module Set = Set.Make (Package)
