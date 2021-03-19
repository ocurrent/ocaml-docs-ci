type t = Package.t

let pp f (t : t) = Fmt.pf f "%a" Package.pp t

let compare (a : t) (b : t) = Package.compare a b

module StringSet = Set.Make (String)

let worthiness t = t |> Package.universe |> Package.Universe.deps |> List.length

(** The goal is to find the minimal number of jobs that builds all the target packages.
This is actually the Set Cover problem. NP-hard :( let's go greedy. 
https://en.wikipedia.org/wiki/Set_cover_problem

package.version.universe -> 1 2 3 4 5
                task -> 1 [ o x o x o x x x ]
                        2 [ o o x o x o o o ]
                        3 [ o o o x x x x x ]
                        4 [ o x x x o x x x ]

  *)
let schedule ~(targets : t list) : t list =
  let targets_digests = targets |> List.map Package.digest |> StringSet.of_list in
  let targets =
    targets |> List.sort (fun (a : t) b -> Int.compare (worthiness b) (worthiness a))
    (* sort in decreasing order in the number of packages produced by job *)
  in
  let remaining_targets = ref targets_digests in
  let check_and_add_target pkg =
    let set = pkg |> Package.all_deps |> List.map Package.digest |> StringSet.of_list in
    let size_before_update = StringSet.cardinal !remaining_targets in
    remaining_targets := StringSet.diff !remaining_targets set;
    let size_after_update = StringSet.cardinal !remaining_targets in
    size_before_update <> size_after_update
  in
  List.filter check_and_add_target targets
