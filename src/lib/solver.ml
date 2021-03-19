module Git = Current_git

let v ~(opam : Git.Commit.t) (package : OpamPackage.t Current.t) : Package.t Current.t =
  let open Current.Syntax in
  let packages =
    let+ package = package in
    [ OpamPackage.name_to_string package; "ocaml-base-compiler" ]
  in
  let constraints =
    let+ package = package in
    [ (OpamPackage.name_to_string package, OpamPackage.version_to_string package) ]
  in
  let+ packages, commit =
    Current_solver.v ~system:Platform.system ~repo:(Current.return opam) ~packages ~constraints
  and+ root = package in
  Package.make ~commit ~root packages
