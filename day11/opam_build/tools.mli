(** Build tool packages from source. *)

val source_dir_strategy : OpamPackage.t -> Types.build_strategy
(** Build strategy for packages with a local source directory mounted at
    [/home/opam/src]. *)

val plan_tool :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Types.build_env ->
  packages:Day11_opam.Git_packages.t ->
  repos:(string * string) list ->
  ?constraints:OpamPackage.t list ->
  ?pin_dirs:string list ->
  ?doc:bool ->
  ?ocaml_version:OpamPackage.t ->
  ?source_dirs:string OpamPackage.Name.Map.t ->
  ?cache:Hash_cache.t ->
  OpamPackage.t ->
  ( Day11_opam_layer.Tool.t * string OpamPackage.Name.Map.t,
    [> Rresult.R.msg ] )
  result
(** [plan_tool ~sw env benv ~packages ~repos ?cache target] solves [target] via
    solver_worker and creates DAG nodes without building. [pin_dirs] are
    directories of [.opam] files pinned at version [dev]. [constraints] pins
    packages at exact versions. When [cache] is provided, shares hash
    computation with the main build DAG. Returns the tool plan and source_dirs
    for pinned packages. Equivalent to {!solve_tool} followed by
    {!plan_tool_of_result}. *)

val solve_tool :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  repos:(string * string) list ->
  ?constraints:OpamPackage.t list ->
  ?pin_dirs:string list ->
  ?doc:bool ->
  ?ocaml_version:OpamPackage.t ->
  OpamPackage.t ->
  (Day11_solution.Solve_result.t, [> Rresult.R.msg ]) result
(** Solver half of {!plan_tool}: one solver-worker subprocess for one target.
    Exposed so callers can interpose a solution cache (see
    {!Day11_batch.Incremental_solver}) between solving and planning. *)

val plan_tool_of_result :
  Types.build_env ->
  packages:Day11_opam.Git_packages.t ->
  ?source_dirs:string OpamPackage.Name.Map.t ->
  ?cache:Hash_cache.t ->
  OpamPackage.t ->
  Day11_solution.Solve_result.t ->
  ( Day11_opam_layer.Tool.t * string OpamPackage.Name.Map.t,
    [> Rresult.R.msg ] )
  result
(** DAG-planning half of {!plan_tool}: build the tool's node DAG from an
    already-obtained (possibly cached) solve result. *)

val build_tool :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Types.build_env ->
  ?np:int ->
  packages:Day11_opam.Git_packages.t ->
  repos:(string * string) list ->
  ?constraints:OpamPackage.t list ->
  ?pin_dirs:string list ->
  ?doc:bool ->
  ?ocaml_version:OpamPackage.t ->
  ?source_dirs:string OpamPackage.Name.Map.t ->
  ?mounts:Day11_container.Mount.t list ->
  OpamPackage.t ->
  (Day11_opam_layer.Tool.t, [> Rresult.R.msg ]) result
(** [build_tool ~sw env benv ?np ~packages ~repos target] solves and builds
    [target] and all its dependencies via solver_worker subprocesses. [pin_dirs]
    are directories of [.opam] files pinned at version [dev]. [constraints] pins
    packages at exact versions. [source_dirs] maps pinned package names to local
    source directories that are mounted into the build container. *)

val read_pins_from_dir :
  string -> (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t
(** [read_pins_from_dir dir] reads all [.opam] files from [dir] and returns a
    pins map with version ["dev"]. *)

val build_tool_from_repo :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  Types.build_env ->
  ?np:int ->
  packages:Day11_opam.Git_packages.t ->
  repos:(string * string) list ->
  ?ocaml_version:OpamPackage.t ->
  ?mounts:Day11_container.Mount.t list ->
  ?extra_repo_dirs:string list ->
  ?extra_target_names:string list ->
  repo_dir:string ->
  target_name:string ->
  unit ->
  (Day11_opam_layer.Tool.t, [> Rresult.R.msg ]) result
(** [build_tool_from_repo ~sw env benv ~packages ~repos ~repo_dir
     ?extra_repo_dirs ~target_name ()] reads [.opam] files from [repo_dir] and
    each [extra_repo_dirs], pins all packages found to dev, and builds
    [target_name.dev] with [~doc:false]. Use for tool builds from local
    checkouts. *)
