(* Blessing — select canonical documentation per package.

   When the same package appears in multiple solutions, pick the "best"
   universe for each package's canonical docs.

   Heuristic (from ocaml-docs-ci):
   1. Maximize deps_count (richer docs with optional deps resolved)
   2. Maximize revdeps_count (stability: fewer blessing changes) *)

let universe_hash_of_deps = Day11_solution.Universe.of_deps

let compute_blessings (solutions : (OpamPackage.t * Day11_solution.Deps.t) list)
    =
  (* Expand direct deps → transitive deps for each solution *)
  let solutions =
    List.map
      (fun (target, sol) -> (target, Day11_solution.Deps.transitive_deps sol))
      solutions
  in
  (* Step 1: Compute revdeps counts across all solutions *)
  let revdeps_counts : (OpamPackage.t, int) Hashtbl.t = Hashtbl.create 256 in
  List.iter
    (fun (_target, trans_deps) ->
      OpamPackage.Map.iter
        (fun _pkg deps ->
          OpamPackage.Set.iter
            (fun dep ->
              let c =
                try Hashtbl.find revdeps_counts dep with Not_found -> 0
              in
              Hashtbl.replace revdeps_counts dep (c + 1))
            deps)
        trans_deps)
    solutions;
  (* Step 2: For each package, collect distinct universes with metrics *)
  (* Entries: (uhash, deps_count, revdeps_count, compiler_version) *)
  let compiler_names =
    List.map OpamPackage.Name.of_string
      [ "ocaml-base-compiler"; "ocaml-variants"; "ocaml-system" ]
  in
  let find_compiler_version solution =
    OpamPackage.Map.fold
      (fun pkg _deps acc ->
        match acc with
        | Some _ -> acc
        | None ->
            if
              List.exists
                (OpamPackage.Name.equal (OpamPackage.name pkg))
                compiler_names
            then Some (OpamPackage.version pkg)
            else None)
      solution None
  in
  let pkg_universes :
      ( OpamPackage.t,
        (Day11_solution.Universe.t * int * int * OpamPackage.Version.t option)
        list )
      Hashtbl.t =
    Hashtbl.create 256
  in
  List.iter
    (fun (_target, trans_deps) ->
      let compiler_v = find_compiler_version trans_deps in
      OpamPackage.Map.iter
        (fun pkg deps ->
          let uhash = universe_hash_of_deps deps in
          let deps_count = OpamPackage.Set.cardinal deps in
          let revdeps_count =
            try Hashtbl.find revdeps_counts pkg with Not_found -> 0
          in
          let existing =
            try Hashtbl.find pkg_universes pkg with Not_found -> []
          in
          if
            not
              (List.exists
                 (fun (h, _, _, _) -> Day11_solution.Universe.equal h uhash)
                 existing)
          then
            Hashtbl.replace pkg_universes pkg
              ((uhash, deps_count, revdeps_count, compiler_v) :: existing))
        trans_deps)
    solutions;
  (* Step 3: For each package, pick the best universe.
     Heuristic: maximize deps_count, then revdeps_count, then
     compiler version (prefer newer). *)
  let compare_version a b =
    match (a, b) with
    | Some va, Some vb -> OpamPackage.Version.compare va vb
    | Some _, None -> 1
    | None, Some _ -> -1
    | None, None -> 0
  in
  let blessed_universe : (OpamPackage.t, Day11_solution.Universe.t) Hashtbl.t =
    Hashtbl.create 256
  in
  Hashtbl.iter
    (fun pkg entries ->
      let best_hash, _, _, _ =
        List.fold_left
          (fun ((_, bdc, brc, bv) as best) ((_, dc, rc, v) as entry) ->
            if dc > bdc then entry
            else if dc = bdc && rc > brc then entry
            else if dc = bdc && rc = brc && compare_version v bv > 0 then entry
            else best)
          (List.hd entries) (List.tl entries)
      in
      Hashtbl.replace blessed_universe pkg best_hash)
    pkg_universes;
  (* Step 4: Generate per-target blessing maps *)
  List.map
    (fun (target, trans_deps) ->
      let map =
        OpamPackage.Map.mapi
          (fun pkg deps ->
            let uhash = universe_hash_of_deps deps in
            let blessed_uhash = Hashtbl.find blessed_universe pkg in
            Day11_solution.Universe.equal uhash blessed_uhash)
          trans_deps
      in
      (target, map))
    solutions

let is_blessed map pkg =
  match OpamPackage.Map.find_opt pkg map with Some b -> b | None -> false

let compute_blessed_universes
    (solutions : (OpamPackage.t * Day11_solution.Deps.t) list) =
  (* Reuse compute_blessings logic — extract blessed universe per package *)
  let blessings = compute_blessings solutions in
  let trans_solutions =
    List.map
      (fun (target, sol) -> (target, Day11_solution.Deps.transitive_deps sol))
      solutions
  in
  let blessed : (OpamPackage.t, Day11_solution.Universe.t) Hashtbl.t =
    Hashtbl.create 256
  in
  List.iter
    (fun (target, map) ->
      let trans_deps = List.assoc target trans_solutions in
      OpamPackage.Map.iter
        (fun pkg is_blessed ->
          if is_blessed && not (Hashtbl.mem blessed pkg) then
            match OpamPackage.Map.find_opt pkg trans_deps with
            | Some deps ->
                Hashtbl.replace blessed pkg (universe_hash_of_deps deps)
            | None -> ())
        map)
    blessings;
  blessed
