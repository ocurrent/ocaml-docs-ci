(** Memoized hash computation for layer cache keys.

    Caches opam file hashes and layer hashes to avoid redundant
    computation when building many packages with shared dependencies. *)

type t
(** A hash cache instance. *)

val create :
  find_opam:(OpamPackage.t -> OpamFile.OPAM.t option) ->
  ?find_oid:(OpamPackage.t -> string option) ->
  ?patches:Patches.t -> unit -> t
(** [create ~find_opam ?patches ()] creates a new hash cache.
    When [patches] is provided, patch content is incorporated into
    package hashes so patched builds get distinct cache keys.

    When [find_oid] (package -> its version-dir tree OID at the
    current repo state) is provided, effective-part digests are served
    from a process-global cache keyed by package and validated by the
    OID — unchanged packages cost no opam read or parse even across
    Profile_ctx reloads; only versions whose tree OID moved are
    re-parsed. The cache is keyed by package identity, never by bare
    OID: byte-identical twin package dirs (templated multi-package
    releases, re-releases) share a tree OID but not a digest — the
    digest includes the name/version stamped in at parse time. *)

val pkg_opam_hash : t -> OpamPackage.t -> string

val layer_hash : t -> base_hash:string -> OpamPackage.t list -> string
(** [layer_hash t ~base_hash pkgs] returns a hash for a build layer.
    Depends on the base image hash and each package's effective opam
    content. Memoized per package list. *)
