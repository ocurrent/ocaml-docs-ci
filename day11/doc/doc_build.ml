module Build = Day11_opam_layer.Build
module Installed_files = Day11_opam_layer.Installed_files

let odoc_bin = "/home/opam/doc-tools/bin/odoc"
let odoc_md_bin = "/home/opam/doc-tools/bin/odoc-md"

type doc_config = {
  driver_tool : Day11_opam_layer.Tool.t;
  odoc_tool : Day11_opam_layer.Tool.t;
  os_dir : Fpath.t;
  blessed : bool;
}

let has_documentable_libs layer_dir =
  Installed_files.scan_libs ~layer_dir <> []

(* Re-exports from {!Day11_opam_build.Compiler_pkg} so doc-side
   code that already imports [Day11_doc.Doc_build] keeps working. *)
let concrete_compiler_names = Day11_opam_build.Compiler_pkg.names
let is_compiler_pkg = Day11_opam_build.Compiler_pkg.is_compiler
let check_stdlib_installed = Day11_opam_build.Compiler_pkg.check_stdlib_installed

(* Solver-based check: a package is "ocaml" (i.e. a real OCaml
   library worth documenting) iff its solved deps include the
   virtual [ocaml] package, OR it's the real compiler
   ({!is_compiler_pkg}). The compiler IS the [ocaml] provider so it
   doesn't depend on [ocaml] itself; admitting it lets stdlib's
   [.cmti] files go through odoc compile and downstream [Stdlib.*]
   xrefs resolve. [ocaml-options-*], [ocaml-config], and [conf-*]
   system packages still don't depend on [ocaml] and so are skipped.

   The [ocaml] dependency is checked over the *transitive* build
   closure, not just [node.deps]: packages like [dune-rpc]/[dune-rpc-lwt]
   reach [ocaml] only through a dep (e.g. [lwt], [stdune]) because their
   own opam file doesn't list [ocaml] directly. A direct-deps-only check
   dropped every such package's doc nodes. [node.deps] forms a DAG
   ({!Build.t}), so we walk it with a visited-set keyed on layer hash. *)
let is_ocaml_package (node : Build.t) =
  let ocaml = OpamPackage.Name.of_string "ocaml" in
  is_compiler_pkg node.pkg
  || begin
    let seen = Hashtbl.create 64 in
    let rec reaches (n : Build.t) =
      List.exists (fun (d : Build.t) ->
        OpamPackage.Name.equal (OpamPackage.name d.pkg) ocaml
        || (not (Hashtbl.mem seen d.hash)
            && (Hashtbl.replace seen d.hash (); reaches d))
      ) n.deps
    in
    reaches node
  end

(** Create tool binary mounts from driver and odoc tools. *)
let make_tool_mounts ~os_dir ~(driver_tool : Day11_opam_layer.Tool.t)
    ~(odoc_tool : Day11_opam_layer.Tool.t) =
  let find name builds =
    let switch = Day11_opam_build.Types.switch in
    match List.find_map (fun (bl : Build.t) ->
      let bin = Fpath.(Build.dir ~os_dir bl / "fs" / "home" / "opam"
        / ".opam" / switch / "bin" / name) in
      if Bos.OS.File.exists bin |> Result.get_ok then Some bin
      else None
    ) builds with
    | Some p -> p
    | None -> failwith (Printf.sprintf "Doc tool binary %s not found" name)
  in
  let mount src dst = Day11_container.Mount.bind_ro
    ~src:(Fpath.to_string src) dst in
  [ mount (find "odoc" odoc_tool.builds) "/home/opam/doc-tools/bin/odoc";
    mount (find "odoc-md" driver_tool.builds) "/home/opam/doc-tools/bin/odoc-md";
    mount (find "odoc_driver_voodoo" driver_tool.builds)
      "/home/opam/doc-tools/bin/odoc_driver_voodoo";
    mount (find "sherlodoc" driver_tool.builds)
      "/home/opam/doc-tools/bin/sherlodoc";
    (* [ocamlobjinfo] is what voodoo invokes via [Eio.Process] to
       extract source-file references from [.cmt] files. Comes from
       the same compiler that built the per-target [odoc], so it
       lives somewhere in [odoc_tool.builds] (any layer that
       installed [ocaml-compiler.X.Y]). For most packages the
       binary is also reachable via the build_deps closure putting
       the compiler on PATH; for self-documenting [ocaml-compiler]
       it isn't (build_deps is empty), so this dedicated mount is
       the uniform fix. *)
    mount (find "ocamlobjinfo" odoc_tool.builds)
      "/home/opam/doc-tools/bin/ocamlobjinfo" ]

(** Pre-mount prep for doc containers: create /home/opam/odoc-out and
    /home/opam/html mount points, chown to build user. *)
let doc_prep_upper ~sw env ~uid ~gid ~upper ~lowers:_ =
  let mkdir path = Bos.OS.Dir.create ~path:true path |> ignore in
  let odoc_out = Fpath.(upper / "home" / "opam" / "odoc-out") in
  let html = Fpath.(upper / "home" / "opam" / "html") in
  mkdir odoc_out; mkdir html;
  ignore (Day11_sys.Sudo.run ~sw env
    Bos.Cmd.(v "chown" % Printf.sprintf "%d:%d" uid gid
             % Fpath.to_string odoc_out % Fpath.to_string html))

let doc_cleanup ~sw env upper =
  ignore (Day11_sys.Sudo.rm_rf ~sw env
    Fpath.(upper / "home" / "opam" / "doc-tools"))

(** Set up the prep structure and mounts for a doc container.
    Returns [(universe, mounts, prep_dir)] or [None] only on a hard
    [Prep.create_with_mounts] failure. Packages with no installed
    libs and no [.mld] files still go through, with [Prep] dropping
    a stub [index.mld] so [odoc_driver_voodoo] has something to
    process — that produces a real (almost-empty) layer instead of
    the previous "skip with no on-disk artefact" path that left
    [inspect_layer]'s [Layer.is_ok] disagreeing with the
    [layer_status.jsonl] truth source. Caller must clean up
    [prep_dir]. *)
let prepare ~(config : doc_config) ~build_layer ~universe pkg =
  let installed_libs = Installed_files.scan_libs ~layer_dir:build_layer in
  let installed_docs = Installed_files.scan_docs ~layer_dir:build_layer in
  (* [universe] is the package's doc-deps universe ([Universe.of_deps] of
     its transitive doc_deps closure), supplied by the planner. It
     namespaces the prep tree at [prep/universes/<universe>/…], which is
     what voodoo classifies from — so the same build layer compiled under
     two different universes produces two distinct, correctly-namespaced
     outputs. Must NOT be recomputed from the build-layer hash: that
     would collapse every doc-universe of a shared build onto one id. *)
  let tool_mounts = make_tool_mounts ~os_dir:config.os_dir
    ~driver_tool:config.driver_tool ~odoc_tool:config.odoc_tool in
  let prep_dir = Bos.OS.Dir.tmp "day11_doc_%s" |> Result.get_ok in
  match Prep.create_with_mounts ~source_layer_dir:build_layer
    ~dest_layer_dir:prep_dir ~universe ~pkg
    ~installed_libs ~installed_docs with
  | Ok (_prep_root, lib_mounts) ->
    let prep_mount = Day11_container.Mount.bind_ro
      ~src:(Fpath.to_string Fpath.(prep_dir / "prep"))
      "/home/opam/prep" in
    let mounts = tool_mounts @ [ prep_mount ] @ lib_mounts in
    Some (universe, mounts, prep_dir)
  | Error _ -> None

(* Pre-voodoo inventory of what's visible inside the container:
    - existing pkg/lib markers in /home/opam/odoc-out (from mounted dep
      doc layers — match what {!Voodoo.extra_paths} will discover)
    - directories under /home/opam/prep/universes/ (the universes
      voodoo's classify step will look at)
   Output goes to layer.log via the build container's stdout. *)
let debug_inspect_image =
  "echo '=== DEBUG: pre-voodoo image inventory ==='; \
   echo '-- pkg markers (.odoc_pkg_marker) --'; \
   find /home/opam/odoc-out -name '.odoc_pkg_marker' 2>/dev/null | sort; \
   echo '-- lib markers (.odoc_lib_marker) --'; \
   find /home/opam/odoc-out -name '.odoc_lib_marker' 2>/dev/null | sort; \
   echo '-- prep universes (universe/pkg/version) --'; \
   ls -d /home/opam/prep/universes/*/*/*/ 2>/dev/null | sort; \
   echo '=== END DEBUG ==='; "

(* Shared body of the three doc phases (compile / link / doc-all). They
   differ only in the voodoo [actions], the [Doc_meta] phase + recorded
   deps, whether an HTML output dir is bind-mounted, the layer dirs to
   stack, and the error label. Returns the built node on success; the
   wrappers map that to the layer dir (compile/doc-all) or unit (link).
   Layer dirs are stacked build-deps first (build-time tooling) then
   compile layers (.odoc files) — order matters for overlayfs. *)
let run_doc_phase ~sw env benv ~(config : doc_config) ~build_layer ~universe
    ~actions ~phase ~meta_deps ~html_dir ~build_dirs ~label ~hash pkg
    : (Build.t, string) result =
  match prepare ~config ~build_layer ~universe pkg with
  | None -> Error "no documentable libraries"
  | Some (universe, mounts, prep_dir) ->
    (* The HTML output dir is a bind-mount source; runc won't start if it
       doesn't exist, so ensure it (top-level callers usually do too). *)
    let mounts = match html_dir with
      | None -> mounts
      | Some dir ->
        ignore (Bos.OS.Dir.create ~path:true dir);
        mounts @ [ Day11_container.Mount.bind_rw
                     ~src:(Fpath.to_string dir) Odoc_store.container_html ]
    in
    let cmd =
      "export PATH=/home/opam/doc-tools/bin:$PATH && eval $(opam env) && " ^
      debug_inspect_image ^
      Command.odoc_driver_voodoo ~pkg ~universe
        ~blessed:config.blessed ~actions ~odoc_bin ~odoc_md_bin in
    let node : Build.t =
      { hash; pkg; deps = []; universe = Day11_solution.Universe.dummy } in
    let on_extract ~layer_dir ~success:_ =
      let dm : Doc_meta.t =
        { package = OpamPackage.to_string pkg; phase; deps = meta_deps } in
      ignore (Doc_meta.save layer_dir dm)
    in
    let result =
      match Day11_opam_build.Build_layer.build ~sw env benv
              ~opam_repositories:[] ~mounts ~build_dirs
              ~prep_upper:(doc_prep_upper ~sw env ~uid:benv.uid ~gid:benv.gid)
              ~on_extract node
              ~strategy:{ cmd; cleanup = doc_cleanup } () with
      | Day11_opam_build.Types.Success _ -> Ok node
      | _ ->
        Error (Printf.sprintf "%s failed for %s" label
          (OpamPackage.to_string pkg))
    in
    ignore (Day11_sys.Sudo.rm_rf ~sw env prep_dir);
    result

let compile ~sw env benv ~(config : doc_config) ~build_layer ~universe
    ~build_deps_layers ~dep_compile_layers ~hash pkg =
  run_doc_phase ~sw env benv ~config ~build_layer ~universe
    ~actions:"compile-only" ~phase:Doc_meta.Compile
    ~meta_deps:(List.map Fpath.basename dep_compile_layers)
    ~html_dir:None
    ~build_dirs:(build_deps_layers @ dep_compile_layers)
    ~label:"compile" ~hash pkg
  |> Result.map (fun node ->
       Day11_opam_layer.Build.dir ~os_dir:config.os_dir node)

let link ~sw env benv ~(config : doc_config) ~build_layer ~universe
    ~build_deps_layers ~compile_layer ~dep_compile_layers ~html_dir ~hash pkg =
  run_doc_phase ~sw env benv ~config ~build_layer ~universe
    ~actions:"link-and-gen" ~phase:Doc_meta.Link ~meta_deps:[]
    ~html_dir:(Some html_dir)
    ~build_dirs:(build_deps_layers @ (compile_layer :: dep_compile_layers))
    ~label:"link" ~hash pkg
  |> Result.map ignore

let doc_all ~sw env benv ~(config : doc_config) ~build_layer ~universe
    ~build_deps_layers ~dep_compile_layers ~html_dir ~hash pkg =
  run_doc_phase ~sw env benv ~config ~build_layer ~universe
    ~actions:"all" ~phase:Doc_meta.Doc_all
    ~meta_deps:(List.map Fpath.basename dep_compile_layers)
    ~html_dir:(Some html_dir)
    ~build_dirs:(build_deps_layers @ dep_compile_layers)
    ~label:"doc-all" ~hash pkg
  |> Result.map (fun node ->
       Day11_opam_layer.Build.dir ~os_dir:config.os_dir node)

