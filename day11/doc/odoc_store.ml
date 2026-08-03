type pkg_loc = { pkg : OpamPackage.t; universe : string; blessed : bool }

let rel_path loc =
  let name = OpamPackage.Name.to_string (OpamPackage.name loc.pkg) in
  let version = OpamPackage.Version.to_string (OpamPackage.version loc.pkg) in
  if loc.blessed then Fpath.(v "p" / name / version)
  else Fpath.(v "u" / loc.universe / name / version)

let container_html = "/home/opam/html"
