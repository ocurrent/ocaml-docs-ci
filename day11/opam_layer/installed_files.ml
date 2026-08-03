let scan_dir ~keep base_dir =
  let result = ref [] in
  let rec walk prefix dir =
    try
      let dir_s = Fpath.to_string dir in
      if Sys.file_exists dir_s && Sys.is_directory dir_s then
        Sys.readdir dir_s
        |> Array.iter (fun name ->
            let full_path = Fpath.(dir / name) in
            let rel_path = if prefix = "" then name else prefix ^ "/" ^ name in
            try
              if Sys.is_directory (Fpath.to_string full_path) then
                walk rel_path full_path
              else if keep rel_path then result := rel_path :: !result
            with Sys_error _ -> ())
    with Sys_error _ -> ()
  in
  walk "" base_dir;
  List.sort String.compare !result

let lib_extensions =
  [ ".cmi"; ".cmti"; ".cmt"; ".cma"; ".cmxa"; ".cmx"; ".ml"; ".mli" ]

let lib_filenames = [ "META"; "dune-package" ]

let scan_libs ~layer_dir =
  let lib_dir =
    Fpath.(layer_dir / "fs" / "home" / "opam" / ".opam" / "default" / "lib")
  in
  scan_dir lib_dir ~keep:(fun rel_path ->
      let name = Filename.basename rel_path in
      List.exists (fun ext -> Filename.check_suffix name ext) lib_extensions
      || List.mem name lib_filenames)

(* Which files under the switch's [doc/] the doc build actually needs.

   [odoc_driver_voodoo] classifies the prep tree with [Opam.classify_docs],
   which only ever looks at three shapes:

     - [doc/<pkg>/odoc-pages/**]  — [.mld] pages plus any sibling assets
     - [doc/<pkg>/odoc-assets/**] — assets, remapped to [_assets/]
     - [doc/<pkg>/<file>]         — "other docs"; of these only [.md] is
                                    rendered (as a page, via [odoc-md]),
                                    everything else is logged and dropped

   plus [doc/<pkg>/odoc-config.sexp], which the driver reads directly.

   The top-level [.md] case is what [voodoo-prep] used to give us for free:
   it copied the whole of the switch's [doc/] tree, and dune installs
   [README.md]/[CHANGES.md]/[LICENSE.md] as [doc:] files, so they landed at
   [doc/<pkg>/*.md] and got picked up. Restricting the scan to [.mld] lost
   those pages. We stay selective rather than copying [doc/] wholesale
   (some packages install whole manuals there) but cover everything the
   driver can consume.

   [.mld] is accepted at any depth for backwards compatibility with the
   previous scan; the driver only honours the [odoc-pages] ones. *)
let doc_is_relevant rel_path =
  let name = Filename.basename rel_path in
  Filename.extension name = ".mld"
  || name = "odoc-config.sexp"
  ||
  match String.split_on_char '/' rel_path with
  | _pkg :: ("odoc-pages" | "odoc-assets") :: _ :: _ -> true
  | [ _pkg; _ ] -> Filename.extension name = ".md"
  | _ -> false

let scan_docs ~layer_dir =
  let doc_dir =
    Fpath.(layer_dir / "fs" / "home" / "opam" / ".opam" / "default" / "doc")
  in
  scan_dir doc_dir ~keep:doc_is_relevant
