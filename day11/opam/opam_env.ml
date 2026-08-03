let std_env ?(ocaml_native = true) ?sys_ocaml_version ?opam_version ~arch ~os
    ~os_distribution ~os_family ~os_version () = function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version" ->
      Some
        (OpamVariable.S
           (Option.value ~default:OpamVersion.(to_string current) opam_version))
  | "sys-ocaml-version" ->
      sys_ocaml_version |> Option.map (fun v -> OpamTypes.S v)
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "enable-ocaml-beta-repository" -> None
  | _ -> None
