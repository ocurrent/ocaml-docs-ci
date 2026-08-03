(* Names of packages that are the real compiler — i.e. the package
   whose build installs [lib/ocaml/stdlib*.cmti]. Mainline split the
   real compiler out of [ocaml-base-compiler] into [ocaml-compiler]
   at 5.3.0; [oxcaml-compiler] is the oxcaml equivalent. We document
   these so downstream cross-references like [Stdlib.Format] resolve. *)
let names =
  List.map OpamPackage.Name.of_string
    [ "ocaml-compiler"; "ocaml-base-compiler"; "oxcaml-compiler" ]

let v_5_3_0 = OpamPackage.Version.of_string "5.3.0"

(** Predicate: is [pkg] the real compiler for its release line? *)
let is_compiler (pkg : OpamPackage.t) : bool =
  let name = OpamPackage.name_to_string pkg in
  let version = OpamPackage.version pkg in
  let cmp = OpamPackage.Version.compare in
  match name with
  | "oxcaml-compiler" -> true
  | "ocaml-compiler" -> cmp version v_5_3_0 >= 0
  | "ocaml-base-compiler" -> cmp version v_5_3_0 < 0
  | _ -> false

(** Post-build invariant for compiler packages: their build layer should contain
    [lib/ocaml/stdlib.cmti]. Returns [Ok ()] for non-compiler packages, doc
    layers (which have [odoc-out/] rather than [lib/ocaml/]), or when the file
    is present. Returns [Error] when the package was {e expected} to install
    stdlib but didn't. *)
let check_stdlib_installed ~build_layer (pkg : OpamPackage.t) =
  if not (is_compiler pkg) then Ok ()
  else
    let odoc_out = Fpath.(build_layer / "fs" / "home" / "opam" / "odoc-out") in
    (* Doc-phase layers (compile / doc-all / link) reuse
       [Container_backend.build] but produce [odoc-out/], not
       [lib/ocaml/]. Skip the stdlib check for those — the real
       package build's layer is what we actually want to validate. *)
    if Bos.OS.Dir.exists odoc_out |> Result.value ~default:false then Ok ()
    else
      let stdlib_cmti =
        Fpath.(
          build_layer
          / "fs"
          / "home"
          / "opam"
          / ".opam"
          / "default"
          / "lib"
          / "ocaml"
          / "stdlib.cmti")
      in
      if Bos.OS.File.exists stdlib_cmti |> Result.value ~default:false then
        Ok ()
      else
        Rresult.R.error_msgf
          "%s: classified as a compiler package but did not install %a — \
           downstream Stdlib xrefs will not resolve. Either the package's \
           build is broken or [Compiler_pkg.is_compiler] needs updating."
          (OpamPackage.to_string pkg)
          Fpath.pp stdlib_cmti
