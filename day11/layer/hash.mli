(** Layer hash computation for cache identity.

    A layer hash uniquely identifies a layer's content based on its inputs. Two
    layers with the same hash are assumed identical and can be reused from
    cache. *)

val of_strings : string list -> string
(** [of_strings parts] computes a hex digest from the concatenation of [parts].
    Used as the building block for all hash functions. *)

val base_hash : image:string -> string
(** [base_hash ~image] returns a hash identifying the base image. *)

val layer_hash :
  base_hash:string -> dep_hashes:string list -> pkg:string -> string
(** [layer_hash ~base_hash ~dep_hashes ~pkg] computes the cache key for a build
    layer. Depends on the base image, all transitive dependency layer hashes,
    and the package being built. *)
