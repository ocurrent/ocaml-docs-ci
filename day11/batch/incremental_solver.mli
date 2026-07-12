(** Incremental solver — solution caching and reuse.

    Caches solver results keyed by opam-repo SHA. When the repo
    advances, reuses solutions whose examined package set has no
    overlap with the set of changed packages.

    {1 On-disk format}

    Each cache entry is a single JSON file at
    [<solutions_dir>/<pkg>.<ver>.json]. The package name+version is
    encoded in the filename, not (or not only) in the JSON body —
    this keeps the body symmetric with the [cache_key] envelope used
    by upstream consumers like ocaml-docs-ci, which write files
    without a [package] field.

    The optional [cache_key] field carries an opaque cache-validity
    fingerprint. day11 CLI never sets it; ocaml-docs-ci sets it to
    [hash(compiler ∥ commit ∥ repos_digest)] so a repo or compiler
    bump invalidates only the affected entries. {!is_cache_key_valid}
    is the validity check — entries without a [cache_key] are always
    considered valid (legacy/CLI behaviour). *)

type cached_solution = {
  package : OpamPackage.t;
  result : Day11_solution.Solve_result.t;
  cache_key : string option;
}
(** A solved result. The examined set used for reusability checks
    lives inside {!Day11_solution.Solve_result.t}. *)

type cached_failure = {
  package : OpamPackage.t;
  error : string;
  examined : OpamPackage.Name.Set.t;
  cache_key : string option;
}

type cache_entry =
  | Cached_solution of cached_solution
  | Cached_failure of cached_failure

val cache_key_of : cache_entry -> string option
(** Project the [cache_key] field from either constructor. *)

val is_cache_key_valid :
  expected:string option -> cache_entry -> bool
(** [is_cache_key_valid ~expected entry] returns [true] when
    [expected] is [None] (caller doesn't care), or when the entry has
    no [cache_key] (legacy entry, always accepted), or when the
    entry's [cache_key] equals [expected]. *)

val save : Fpath.t -> cache_entry -> (unit, [> Rresult.R.msg ]) result
(** [save path entry] writes a cache entry (solution or failure) to
    a JSON file. Embeds the entry's [cache_key] when set. *)

val load : Fpath.t -> (cache_entry, [> Rresult.R.msg ]) result
(** [load path] reads a cache entry from a JSON file. The package is
    taken from the JSON body if present; otherwise the file's
    basename (minus [.json]) is parsed as [name.version]. *)

val reuse_solutions :
  ?expected_cache_key:string ->
  ?rekey_to:string ->
  solutions_cache_dir:Fpath.t ->
  previous_dir:Fpath.t ->
  changed_packages:OpamPackage.Name.Set.t ->
  packages:string list ->
  unit ->
  int
(** [reuse_solutions ~solutions_cache_dir ~previous_dir
    ~changed_packages ~packages ()] hardlinks reusable solutions from
    [previous_dir] into [solutions_cache_dir]. A solution is reusable
    when its examined set does not intersect [changed_packages].
    Returns the number of solutions reused.

    [?expected_cache_key] additionally requires each previous entry to
    pass {!is_cache_key_valid} against it — pass the cache key the
    previous snapshot's entries were written with, so entries left by
    an interrupted run under different inputs are not trusted.

    [?rekey_to] switches from hardlinking to rewriting: each reused
    entry is re-saved with the given cache key so the destination
    snapshot's own validity check accepts it. In this mode an existing
    destination file is overwritten (the caller is expected to pass
    only targets it knows to be missing or stale). Cached {e failures}
    are carried like solutions — an untouched examined set means the
    failure provably still holds; whether that short-circuits the
    re-solve is the consumer's call.

    Reuse composes across snapshots: an entry reused into snapshot N
    was checked against the N-1→N diff and rekeyed, so checking the
    N→N+1 diff at the next hop maintains validity inductively. *)

val tool_solutions_dirname : string
(** Per-snapshot subdirectory holding cached doc-tool solves
    ([<pkg>@<compiler>.json] entries — the compiler pin is encoded in
    the filename; {!load} reads the package from the JSON body). *)

val tool_cache_key : repos:(string * string) list -> string
(** [tool_cache_key ~repos] is the cache key stamped on tool-solve
    entries: a digest of the [(path, sha)] repo set alone. The
    compiler rides in the entry's filename, so one key covers every
    entry in a snapshot's {!tool_solutions_dirname}. *)

val find_previous_sha_dir :
  Fpath.t -> current_sha:string -> Fpath.t option
(** [find_previous_sha_dir base ~current_sha] finds the most recently
    modified SHA directory under [base] that is not [current_sha]. *)
