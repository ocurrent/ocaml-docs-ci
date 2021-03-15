type t = { root : Package.t; pkgs : Package.t list }

let pp f t = Fmt.pf f "%a" Package.pp t.root

let compare a b = Package.compare a.root b.root

module StringSet = Set.Make (String)

(** The goal is to find the minimal number of jobs that builds all our blessed packages.
This is actually the Set Cover problem. NP-hard :( let's go greedy. 
https://en.wikipedia.org/wiki/Set_cover_problem

package.version.universe -> 1 2 3 4 5
                task -> 1 [ o x o x o x x x ]
                        2 [ o o x o x o o o ]
                        3 [ o o o x x x x x ]
                        4 [ o x x x o x x x ]

  *)
let schedule ~(targets : t list) ~(blessed : Package.Blessed.t list) : t list =
  let blessed = blessed |> List.map Package.digest |> StringSet.of_list in
  let is_blessed pkg = StringSet.mem (Package.digest pkg) blessed in
  let targets =
    targets
    |> List.map (fun x -> { x with pkgs = List.filter is_blessed x.pkgs })
    |> List.sort (fun a b -> Int.compare (List.length b.pkgs) (List.length a.pkgs))
    (* sort in decreasing order in the number of blessed packages produced by job *)
  in
  let remaining_blessed = ref blessed in
  let check_and_add_target { pkgs; _ } =
    let set = pkgs |> List.map Package.digest |> StringSet.of_list in
    let size_before_update = StringSet.cardinal !remaining_blessed in
    remaining_blessed := StringSet.diff !remaining_blessed set;
    let size_after_update = StringSet.cardinal !remaining_blessed in
    size_before_update <> size_after_update
  in
  List.filter check_and_add_target targets
