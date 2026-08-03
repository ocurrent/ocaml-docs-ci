(** Build JTW tools, generate artifacts, and assemble output.

    Builds [js_top_worker-bin] from a local checkout for each unique compiler
    version, runs per-package and per-solution generation, then assembles the
    content-hashed output directory. *)

val build_per_compiler :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  np:int ->
  packages:Day11_opam.Git_packages.t ->
  repos:(string * string) list ->
  mounts:Day11_container.Mount.t list ->
  extra_repo_dirs:string list ->
  repo_dir:string ->
  solutions:(OpamPackage.t * Day11_solution.Deps.t) list ->
  (OpamPackage.t * Day11_opam_layer.Tool.t) list
(** Build JTW tools for each compiler version. Returns the built tools. *)

val build_and_run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  np:int ->
  os_dir:Fpath.t ->
  packages:Day11_opam.Git_packages.t ->
  repos:(string * string) list ->
  mounts:Day11_container.Mount.t list ->
  extra_repo_dirs:string list ->
  repo_dir:string ->
  output:string ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Deps.t) list ->
  unit
(** Build tools, generate per-package artifacts and worker.js, then assemble the
    output directory at [output]. *)
