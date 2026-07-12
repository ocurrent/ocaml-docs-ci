(** Memoized hash computation for layer cache keys.

    Caches opam file hashes and layer hashes to avoid redundant
    computation when building many packages with shared dependencies. *)

type t
(** A hash cache instance. *)

module Digest_store : sig
  type t
  (** Persistent [version-dir tree OID -> effective-part digest] map.
      Content-addressed, so entries are valid forever and the store
      can be shared across profiles and processes. Backed by an
      append-only text file ("<oid> <digest>" lines). *)

  val load : Fpath.t -> t
  (** Load (or lazily create) the store at the given path. Unreadable
      or torn lines are skipped. *)
end

val create :
  find_opam:(OpamPackage.t -> OpamFile.OPAM.t option) ->
  ?find_oid:(OpamPackage.t -> string option) ->
  ?digest_store:Digest_store.t ->
  ?patches:Patches.t -> unit -> t
(** [create ~find_opam ?patches ()] creates a new hash cache.
    When [patches] is provided, patch content is incorporated into
    package hashes so patched builds get distinct cache keys.

    When both [find_oid] (package -> its version-dir tree OID at the
    current repo state) and [digest_store] are provided, effective-part
    digests are served from the persistent store keyed by OID —
    unchanged packages cost no opam read or parse at all; only
    versions whose tree OID is new are parsed (and then persisted). *)

val pkg_opam_hash : t -> OpamPackage.t -> string

val layer_hash : t -> base_hash:string -> OpamPackage.t list -> string
(** [layer_hash t ~base_hash pkgs] returns a hash for a build layer.
    Depends on the base image hash and each package's effective opam
    content. Memoized per package list. *)
