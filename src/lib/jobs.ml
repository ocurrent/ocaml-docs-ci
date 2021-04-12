type t = { install : Package.t; prep : Package.t list }
(** a job is one package to install, from which a set of prep folders can be derived.*)

let pp f (t : t) = Fmt.pf f "%a" Package.pp t.install

let compare (a : t) (b : t) = Package.compare a.install b.install

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


Note: this solution is not ideal. Prep steps may fail (and are more prone to failure as the 
number of installed packages increases), so the algorithm might select "big" prep jobs
that actually fail to build, and thus removes a large set of good candidates. 
*)
let schedule ~(targets : Package.t list) jobs : t list =
  Printf.printf "Schedule %d\n" (List.length jobs);
  let targets_digests = targets |> List.rev_map Package.digest |> StringSet.of_list in
  let jobs =
    jobs |> List.sort (fun (a : Package.t) b -> Int.compare (worthiness b) (worthiness a))
    (* sort in decreasing order in the number of packages produced by job *)
  in
  let remaining_targets = ref targets_digests in
  let check_and_add_target pkg =
    let set = pkg |> Package.all_deps |> List.map Package.digest |> StringSet.of_list in
    let useful_packages = StringSet.inter !remaining_targets set in
    match StringSet.cardinal useful_packages with
    | 0 -> None
    | _ ->
        remaining_targets := StringSet.diff !remaining_targets set;
        Some { install = pkg; prep = Package.all_deps pkg }
  in
  List.filter_map check_and_add_target jobs
