module Git = Current_git

(* TODO: use an API for that *)
let v ~(opam : Git.Commit.t) (package : OpamPackage.t Current.t) : Package.t Current.t =
  let open Current.Syntax in
  let packages =
    let+ package = package in
    [ OpamPackage.name_to_string package ]
  in
  let constraints =
    let+ package = package in
    OpamPackage.Name.(Map.singleton (OpamPackage.name package) (`Eq, OpamPackage.version package))
  in
  let+ packages =
    Current_solver.v ~system:Platform.system ~repo:(Current.return opam) ~packages ~constraints
  and+ root = package in
  Package.v root packages

let explode ~opam universe =
  let packages = Current.map Universe.deps universe in
  Current.list_map (module O.OpamPackage) (v ~opam) packages
