let needs_separate_link (result : Day11_solution.Solve_result.t) pkg =
  let compile_set =
    match OpamPackage.Map.find_opt pkg result.build_deps with
    | Some deps -> deps
    | None -> OpamPackage.Set.empty
  in
  let link_set =
    match OpamPackage.Map.find_opt pkg result.doc_deps with
    | Some deps -> deps
    | None -> OpamPackage.Set.empty
  in
  not (OpamPackage.Set.equal compile_set link_set)
