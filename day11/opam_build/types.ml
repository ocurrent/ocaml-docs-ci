module Build = Day11_opam_layer.Build
module Tool = Day11_opam_layer.Tool

type build = Build.t
type tool = Tool.t

type build_env = {
  base : Day11_layer.Base.t;
  os_dir : Fpath.t;
  uid : int;
  gid : int;
  cpu_slots : Day11_runner.Cpu_slots.t option;
      (** Optional NUMA-aware cpuset pool. When [Some], every container launch
          acquires a slot and passes its [(cpuset, numa_mems)] into the OCI
          spec. [None] leaves containers unconstrained (legacy behaviour). *)
}

let make_build_env ~base ~os_dir ?(uid = Unix.getuid ()) ?(gid = Unix.getgid ())
    ?cpu_slots () =
  { base; os_dir; uid; gid; cpu_slots }

let packages_dir env = Fpath.(env.os_dir / "packages")

let ensure_dirs env =
  Bos.OS.Dir.create ~path:true env.os_dir |> ignore;
  Bos.OS.Dir.create ~path:true (packages_dir env) |> ignore

let switch = "default"

type build_result =
  | Success of Day11_opam_layer.Build.t
  | Failure of string
  | Dependency_failed
  | No_solution of string
  | Solution of Day11_solution.Deps.t

type build_strategy = {
  cmd : string;
  cleanup : sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Fpath.t -> unit;
}
