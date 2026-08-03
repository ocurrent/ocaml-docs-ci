(** JTW artifact generation and assembly.

    Three phases:
    - {b Per-package}: for each package with findlib libraries, run
      [jtw opam --path <pkg> --no-worker] to produce [.cma.js], [.cmi], [META],
      and [dynamic_cmis.json].
    - {b Worker}: for each solution, run [jtw opam stdlib] with all build layers
      stacked to produce [worker.js] and stdlib artifacts.
    - {b Assembly}: merge per-package and worker outputs into a content-hashed
      directory structure for serving.

    JTW tool binaries are bind-mounted from the tool layer. *)

val run :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Day11_opam_build.Types.build_env ->
  os_dir:Fpath.t ->
  jtw_tools:(OpamPackage.t * Day11_opam_layer.Tool.t) list ->
  nodes:Day11_opam_layer.Build.t list ->
  solutions:(OpamPackage.t * Day11_solution.Deps.t) list ->
  (OpamPackage.t, Day11_opam_layer.Build.t) Hashtbl.t
  * (OpamPackage.t * Day11_opam_layer.Build.t) list
(** [run env benv ~os_dir ~jtw_tools ~nodes ~solutions] generates per-package
    JTW artifacts and per-solution worker.js. Returns
    [(jtw_results, worker_layers)] where [jtw_results] maps packages to their
    JTW layer and [worker_layers] maps compilers to their worker layer. *)

val assemble :
  os_dir:Fpath.t ->
  output:string ->
  jtw_results:(OpamPackage.t, Day11_opam_layer.Build.t) Hashtbl.t ->
  worker_layers:(OpamPackage.t * Day11_opam_layer.Build.t) list ->
  solutions:(OpamPackage.t * Day11_solution.Deps.t) list ->
  unit
(** [assemble ~os_dir ~output ~jtw_results ~worker_layers ~solutions] copies
    artifacts into a content-hashed directory structure at [output]. *)
