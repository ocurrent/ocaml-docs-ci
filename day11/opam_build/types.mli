(** Build types.

    Core type definitions shared across the opam-build pipeline: environment
    configuration, result variants, and build strategies. *)

module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool

type build = Build.t
(** Alias for {!Day11_opam_layer.Build.t}. *)

type tool = Tool.t
(** Alias for {!Day11_opam_layer.Tool.t}. *)

type build_env = {
  base : Day11_layer.Base.t;  (** Root filesystem layer. *)
  os_dir : Fpath.t;  (** Per-OS cache directory. *)
  uid : int;  (** UID of the non-root user in the container. *)
  gid : int;  (** GID of the non-root user in the container. *)
  cpu_slots : Day11_runner.Cpu_slots.t option;
      (** Optional NUMA-aware cpu slot pool. When [Some], every container launch
          acquires a slot and pins its cpuset / NUMA memory node; [None] leaves
          containers unconstrained. *)
}
(** Invariant build parameters for a batch run. The opam switch is always
    ["default"]. *)

val make_build_env :
  base:Day11_layer.Base.t ->
  os_dir:Fpath.t ->
  ?uid:int ->
  ?gid:int ->
  ?cpu_slots:Day11_runner.Cpu_slots.t ->
  unit ->
  build_env
(** Create a {!type:build_env}. [uid] and [gid] default to the current process's
    UID/GID. *)

val packages_dir : build_env -> Fpath.t
(** Return the [packages/] subdirectory of {!field:build_env.os_dir}. *)

val ensure_dirs : build_env -> unit
(** Create [os_dir] and [packages/] if they do not exist. *)

val switch : string
(** The opam switch name used in all builds (["default"]). *)

(** Outcome of a build or solve step. *)
type build_result =
  | Success of Day11_opam_layer.Build.t  (** Build completed successfully. *)
  | Failure of string  (** Build failed with an error message. *)
  | Dependency_failed  (** A dependency's build failed. *)
  | No_solution of string  (** Solver could not find a solution. *)
  | Solution of Day11_solution.Deps.t
      (** Solver produced a solution (not yet built). *)

type build_strategy = {
  cmd : string;
  cleanup : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Fpath.t -> unit;
}
(** A build strategy: the command to run inside the container, and a cleanup
    function applied to the upper dir after the build. *)
