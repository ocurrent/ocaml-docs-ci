(** Create prep directory structure and return bind mounts that map build layer
    lib/doc dirs into the prep layout. No file copying — the container sees
    files directly from cached layers. *)
let create_with_mounts ~source_layer_dir ~dest_layer_dir ~universe ~pkg
    ~installed_libs ~installed_docs =
  let switch = "default" in
  let pkg_name = OpamPackage.name_to_string pkg in
  let pkg_version = OpamPackage.version_to_string pkg in
  let prep_root = Fpath.(dest_layer_dir / "prep") in
  let pkg_prep =
    Fpath.(prep_root / "universes" / universe / pkg_name / pkg_version)
  in
  let lib_dest = Fpath.(pkg_prep / "lib") in
  let doc_dest = Fpath.(pkg_prep / "doc") in
  try
    Bos.OS.Dir.create ~path:true lib_dest |> ignore;
    Bos.OS.Dir.create ~path:true doc_dest |> ignore;
    let lib_src =
      Fpath.(
        source_layer_dir / "fs" / "home" / "opam" / ".opam" / switch / "lib")
    in
    (* Collect unique top-level lib subdirs to mount *)
    let lib_dirs =
      installed_libs
      |> List.filter_map (fun rel_path ->
             match String.split_on_char '/' rel_path with
             | dir :: _ -> Some dir
             | [] -> None)
      |> List.sort_uniq String.compare
    in
    let lib_mounts =
      List.filter_map
        (fun dir ->
          let src = Fpath.(lib_src / dir) in
          if Bos.OS.Dir.exists src |> Result.get_ok then (
            (* Create mount point dir in prep *)
            Bos.OS.Dir.create ~path:true Fpath.(lib_dest / dir) |> ignore;
            let container_dest =
              Printf.sprintf "/home/opam/prep/universes/%s/%s/%s/lib/%s"
                universe pkg_name pkg_version dir
            in
            Some
              (Day11_container.Mount.bind_ro ~src:(Fpath.to_string src)
                 container_dest))
          else None)
        lib_dirs
    in
    let doc_src =
      Fpath.(
        source_layer_dir / "fs" / "home" / "opam" / ".opam" / switch / "doc")
    in
    (* Doc files: [.mld] pages, the [odoc-pages]/[odoc-assets] trees, the
       top-level [.md] files ([README.md], [CHANGES.md], [LICENSE.md] — which
       [odoc-md] turns into pages) and [odoc-config.sexp]. Which files are
       relevant is decided by {!Day11_opam_layer.Installed_files.scan_docs};
       here we copy the lot. There are only a handful per package, too few to
       justify a bind mount each. *)
    let any_doc_copied = ref false in
    List.iter
      (fun rel_path ->
        let src = Fpath.(doc_src // v rel_path) in
        let dst = Fpath.(doc_dest // v rel_path) in
        if Bos.OS.File.exists src |> Result.get_ok then (
          Bos.OS.Dir.create ~path:true (Fpath.parent dst) |> ignore;
          Bos.OS.File.read src
          |> Result.get_ok
          |> Bos.OS.File.write dst
          |> ignore;
          any_doc_copied := true))
      installed_docs;
    (* odoc_driver_voodoo exits non-zero when [Voodoo.find_pkg] finds no
       file at all under [prep/universes/<u>/<pkg>/<version>/]. For packages
       that install nothing documentable — ocaml wrappers, [conf-*] binding
       stubs, etc. — we still want voodoo to run and produce a real layer
       (so Layer.is_ok / inspect_layer agree with Layer_status). Drop in a
       one-line stub [.mld] so the package is discoverable. Only needed when
       we copied nothing: a real [.mld], a [README.md] &c. already makes
       [find_pkg] succeed. *)
    if (not !any_doc_copied) && lib_mounts = [] then (
      let stub_dir = Fpath.(doc_dest / pkg_name) in
      Bos.OS.Dir.create ~path:true stub_dir |> ignore;
      let stub = Fpath.(stub_dir / "index.mld") in
      let body =
        Printf.sprintf
          "{0 %s.%s}\n\nThis package installs no documentable libraries.\n"
          pkg_name pkg_version
      in
      Bos.OS.File.write stub body |> ignore);
    Ok (prep_root, lib_mounts)
  with exn ->
    Rresult.R.error_msgf "Prep.create_with_mounts: %s" (Printexc.to_string exn)
