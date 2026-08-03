(** Resolved profile context: profile + everything derived from it.

    A {!Profile.t} is the on-disk stable configuration. A {!t} is what day11
    actually runs against: the profile plus the git package index read from its
    opam repositories, the opam variable environment for its platform, the base
    layer for its distribution + architecture, the
    {!Day11_opam_build.Types.build_env} it produces, and a shared
    {!Day11_opam_build.Hash_cache.t}.

    Callers that today assembled these pieces by hand from a profile —
    [cmd_batch], [cmd_build], [cmd_rerun], the ocaml-docs-ci pipeline —
    construct one [Profile_ctx.t] up front and pass it to
    {!Day11_doc.Generate.build_tools_and_run}, {!plan_doc_dag}, and similar. *)

type t = {
  profile : Profile.t;
  cache_dir : Fpath.t;
      (** Top-level day11 cache root (e.g. [~/.day11/cache]). Typically
          [paths.cache_dir] from [Common.paths]. *)
  os_dir : Fpath.t;
      (** Platform-specific cache dir ([cache_dir / Profile.os_dir_name]). *)
  git_packages : Day11_opam.Git_packages.t;
      (** Loaded opam package index from [profile.opam_repositories]. *)
  repos_with_shas : (string * string) list;
      (** [(repo_path, commit_sha)] pairs, one per repo; SHAs are resolved from
          the current [HEAD] of each repository at [load] time. *)
  opam_env : string -> OpamVariable.variable_contents option;
      (** Opam variable lookup for this profile's platform ([arch],
          [os_distribution], [os_family], [os_version]). *)
  ocaml_version : OpamPackage.t option;
      (** Parsed [profile.compiler] — used as a constraint for the solver when
          set. *)
  driver_compiler : OpamPackage.t option;
      (** Parsed [profile.driver_compiler], or [None] if the profile uses the
          empty-string default. *)
  patches : Day11_opam_build.Patches.t option;
      (** Loaded from [profile.patches_dir], if any. *)
  base : Day11_layer.Base.t;
      (** Base layer. At [load] time, the [hash] is always computable from
          profile fields; the on-disk [dir] may be empty (base image not built
          yet). Use {!ensure_base} to build and populate it. *)
  benv : Day11_opam_build.Types.build_env;
      (** Shared {!Day11_opam_build.Types.build_env} built from [base] +
          [os_dir]. *)
  hash_cache : Day11_opam_build.Hash_cache.t;
      (** Shared hash cache seeded with [find_opam] from [git_packages] and
          [patches]. Passed to {!Day11_opam_build.Dag.build_dag} and to doc tool
          planning so that the same node hashes are computed everywhere. *)
}

val load : Profile.t -> cache_dir:Fpath.t -> t
(** Load profile-derived context: read the opam repositories, resolve commit
    SHAs, build the opam env, compute the base hash, construct [benv] and
    [hash_cache]. Does not touch the network and does not materialise the base
    image on disk. *)

val load_lwt : Profile.t -> cache_dir:Fpath.t -> t Lwt.t
(** Lwt-native version of {!load}. Use from inside an already-running Lwt event
    loop (e.g. an OCurrent Op) to avoid nested [Lwt_main.run] errors in the
    underlying git-unix reads. *)

val ensure_base :
  sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> t -> (t, [> Rresult.R.msg ]) result
(** Materialise the base image on disk if not already cached, returning an
    updated ctx with the resulting [base] populated. Also ensures the cached
    [opam-build] binary exists (built from [profile.opam_build_repo] if set,
    otherwise from upstream), so the base image embeds it. Idempotent: if the
    base layer already exists, returns the ctx unchanged. This is the single
    entry point for triggering a [docker build] of the base image. *)

val with_cpu_slots : t -> Day11_runner.Cpu_slots.t -> t
(** Return [ctx] with its [benv] rebuilt so container launches acquire / release
    slots from [pool] — caps nested build parallelism and pins each container to
    a NUMA-local cpu set. *)

val with_base_digest : t -> string -> t
(** [with_base_digest ctx digest] returns a new context whose
    [profile.base_image_digest] is set to [digest] and whose [base] layer hash
    and [benv] are recomputed against that digest. The on-disk [base.dir] is
    preserved; [ensure_base] must still be called to materialise the new base
    image. Use when an external source (e.g. a periodic docker-manifest-inspect
    OCurrent job) produces a fresh digest that should drive the whole build
    tree. *)

val require_base : t -> (t, [> Rresult.R.msg ]) result
(** Like {!ensure_base}, but never runs [docker build] — returns an error if the
    base image is not already cached. Use from contexts that should fail fast
    when the base image is missing (e.g. ocaml-docs-ci's doc pipeline). *)
