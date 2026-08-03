let compiler_packages =
  List.map OpamPackage.Name.of_string
    [
      "base-bigarray";
      "base-domains";
      "base-effects";
      "base-nnp";
      "base-threads";
      "base-unix";
      "host-arch-x86";
      "host-system-other";
      "ocaml";
      "ocaml-base-compiler";
      "ocaml-compiler";
      "ocaml-config";
      "ocaml-options-vanilla";
      "ocaml-system";
      "ocaml-variants";
    ]

let dump_state packages_dirs state_file =
  try
    let content =
      List.concat_map
        (fun dir ->
          try Sys.readdir (Fpath.to_string dir) |> Array.to_list
          with Sys_error _ -> [])
        packages_dirs
    in
    let packages =
      List.filter_map OpamPackage.of_string_opt content
      |> List.sort_uniq OpamPackage.compare
    in
    let sel_compiler =
      List.filter
        (fun x -> List.mem (OpamPackage.name x) compiler_packages)
        packages
    in
    let s = OpamPackage.Set.of_list packages in
    let new_state =
      {
        OpamTypes.sel_installed = s;
        sel_roots = s;
        sel_pinned = OpamPackage.Set.empty;
        sel_compiler = OpamPackage.Set.of_list sel_compiler;
      }
    in
    OpamFilename.write
      (OpamFilename.raw (Fpath.to_string state_file))
      (OpamFile.SwitchSelections.write_to_string new_state);
    Ok ()
  with exn -> Rresult.R.error_msgf "dump_state: %s" (Printexc.to_string exn)
