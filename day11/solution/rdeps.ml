let find solutions pkg =
  List.fold_left
    (fun acc solution ->
      let trans = Deps.transitive_deps solution in
      OpamPackage.Map.fold
        (fun p deps acc ->
          if OpamPackage.Set.mem pkg deps then OpamPackage.Set.add p acc
          else acc)
        trans acc)
    OpamPackage.Set.empty solutions
