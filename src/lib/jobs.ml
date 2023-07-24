type t = { install : Package.t; prep : Package.t list }
(** A job is one package to install, from which a set of prep folders can be
    derived. *)

let pp f (t : t) = Fmt.pf f "%a" Package.pp t.install
let compare (a : t) (b : t) = Package.compare a.install b.install

module StringSet = Set.Make (String)

let worthiness t = t |> Package.universe |> Package.Universe.deps |> List.length

(** The goal is to find the minimal number of jobs that builds all the target
    packages. This is actually the Set Cover problem. NP-hard :( let's go
    greedy. https://en.wikipedia.org/wiki/Set_cover_problem

    package.version.universe -> 1 2 3 4 5 task -> 1 [ o x o x o x x x ] 2
    [ o o x o x o o o ] 3 [ o o o x x x x x ] 4 [ o x x x o x x x ]

    1) Sort by decreasing universe size, and create a new job as long as there
    is some useful package. 2) For each job, sort by increasing universe size
    and set `prep` as the never previously encountered packages in the universe. *)
let schedule ~(targets : Package.Set.t) jobs : t list =
  Printf.printf "Schedule %d\n" (List.length jobs);
  let targets_digests =
    targets |> Package.Set.to_seq |> Seq.map Package.digest |> StringSet.of_seq
  in
  let jobs =
    jobs
    |> List.rev_map (fun pkg ->
           let install_set =
             pkg
             |> Package.all_deps
             |> List.rev_map Package.digest
             |> StringSet.of_list
           in
           (pkg, install_set))
    |> List.sort (fun (_, s1) (_, s2) ->
           StringSet.cardinal s2 - StringSet.cardinal s1)
    (* Sort in decreasing order in universe size  *)
  in
  let remaining_targets = ref targets_digests in
  let check_and_add_target (pkg, install_set) =
    let useful_packages = StringSet.inter !remaining_targets install_set in
    match StringSet.cardinal useful_packages with
    | 0 -> None
    | _ ->
        remaining_targets := StringSet.diff !remaining_targets useful_packages;
        Some (pkg, install_set)
  in
  let to_install = List.filter_map check_and_add_target jobs in
  let remaining_targets = ref targets_digests in
  let create_job (pkg, install_set) =
    let useful_packages = StringSet.inter !remaining_targets install_set in
    remaining_targets := StringSet.diff !remaining_targets useful_packages;
    {
      install = pkg;
      prep =
        Package.all_deps pkg
        |> List.filter (fun elt ->
               StringSet.mem (Package.digest elt) useful_packages);
    }
  in
  to_install
  |> List.sort (fun (_, s1) (_, s2) ->
         StringSet.cardinal s1 - StringSet.cardinal s2)
  (* Sort in increasing order in universe size *)
  |> List.rev_map create_job
