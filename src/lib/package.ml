type t = { opam : OpamPackage.t; universe : Universe.t }

let universe t = t.universe

let opam t = t.opam

let v opam deps = { opam; universe = Universe.v deps }

module Blessed = struct
  type nonrec t = t
end

let pp f { universe; opam } = Fmt.pf f "%s; %a" (OpamPackage.to_string opam) Universe.pp universe

let compare t t2 =
  match OpamPackage.compare t.opam t2.opam with
  | 0 -> Universe.compare t.universe t2.universe
  | v -> v

let bless (packages : t list) : Blessed.t list =
  let state = ref OpamPackage.Map.empty in
  List.iter (fun package -> state := OpamPackage.Map.add package.opam package !state) packages;
  OpamPackage.Map.values !state
