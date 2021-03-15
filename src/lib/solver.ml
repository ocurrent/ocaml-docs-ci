module Git = Current_git

let v ~(opam : Git.Commit.t) ?(universe : Universe.t Current.t option)
    (package : OpamPackage.t Current.t) : Package.t Current.t =
  let open Current.Syntax in
  let packages =
    let+ package = package in
    [ OpamPackage.name_to_string package ]
  in
  let constraints =
    match universe with
    | None ->
        let+ package = package in
        [ (OpamPackage.name_to_string package, OpamPackage.version_to_string package) ]
    | Some universe ->
        let+ universe = universe in
        universe |> Universe.deps
        |> List.map (fun pkg -> (OpamPackage.name_to_string pkg, OpamPackage.version_to_string pkg))
  in
  let+ (packages, commit) =
    Current_solver.v ~system:Platform.system ~repo:(Current.return opam) ~packages ~constraints
  and+ root = package in
  Package.v root packages commit

let explode ~opam universe =
  Current.collapse ~key:"explode" ~value:"" ~input:universe
  @@
  let packages = Current.map Universe.deps universe in
  Current.list_map (module O.OpamPackage) (v ~universe ~opam) packages
