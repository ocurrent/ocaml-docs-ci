(** Named profiles for day11 analysis configurations.

    A profile captures the stable configuration for an ongoing analysis: which
    opam repositories to use, what overrides to apply, what targets to build,
    and what platform to target. Profiles are stored as JSON files in a profile
    directory (default [~/.day11/profiles/]). *)

(** Target selection on two orthogonal axes: which versions to take, and which
    packages to include. *)
type version_mode =
  | All_versions
  | Latest_n of int
      (** Keep the [n] most-recent (non-avoided) versions of each package.
          [Latest_n 1] = newest only. *)

type name_filter =
  | All_names
  | Names of string list  (** Track only these exact package names. *)

type target_mode = { versions : version_mode; names : name_filter }

type t = {
  name : string;
  opam_repositories : string list;
  odoc_repo : string option;
  opam_build_repo : string option;
  compiler : string option;
  target_mode : target_mode;
  with_doc : bool;
  with_jtw : bool;
  jtw_repo : string option;
  arch : string;
  os_distribution : string;
  os_version : string;
  driver_compiler : string;
  extra_pins : string list;
  pinned_versions : string list;
      (** Hard version pins fed into the solver as [(`Eq, version)] constraints,
          format ["name.version"]. Used to propagate a specific [+ox] (or
          otherwise variant-flavoured) version through a target's transitive
          deps when the solver would otherwise pick a lex-max mainline
          alternative — equivalent in shape to [oi]'s [x-oi-toolchain-roots].
          Empty list = no extra pins beyond [compiler]. *)
  patches_dir : string option;
  base_image_digest : string option;
  base_image_updated : string option;
  html_dir : string option;
      (** Destination for generated HTML when this profile is driven by a doc
          pipeline. [None] = build-only, no docs. *)
}

val save : dir:Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [save ~dir profile] writes [profile] to [dir/<name>.json]. *)

val load : dir:Fpath.t -> name:string -> (t, [> Rresult.R.msg ]) result
(** [load ~dir ~name] reads a profile from [dir/<name>.json].

    Relative [opam_repositories] entries are resolved against the .day11 root
    ([dir]'s parent — [dir] being the [profiles/] dir), so a profile can refer
    to e.g. ["overlays/odoc-master/repo"] without an absolute prefix. Absolute
    entries are kept as-is. *)

val list : dir:Fpath.t -> string list
(** [list ~dir] returns the names of all profiles in [dir]. *)

val delete : dir:Fpath.t -> name:string -> (unit, [> Rresult.R.msg ]) result
(** [delete ~dir ~name] removes the profile file [dir/<name>.json]. *)

val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, [> Rresult.R.msg ]) result

val pp : t Fmt.t
(** Pretty-print a profile for display. *)

(** {1 Derived paths} *)

val os_dir_name : t -> string
(** E.g. ["debian-bookworm-x86_64"]. *)

val default_dir : unit -> Fpath.t
(** [~/.day11] *)

val resolve_repo : day11_dir:Fpath.t -> string -> string
(** [resolve_repo ~day11_dir s] resolves a repository path against the .day11
    root: an absolute [s] is returned unchanged, a relative one is taken
    relative to [day11_dir] (and normalised). {!load} applies this to every
    [opam_repositories] entry; callers that accept repo paths from elsewhere
    (e.g. ocaml-docs-ci's [--remote] / [--github-pin-overlay] CLI args) should
    use it too so the paths resolve identically and line up. *)

val base_image_tag : t -> string
(** E.g. ["debian:bookworm"]. *)

val resolve_base_digest : t -> string option
(** Query the Docker registry for the current image digest. Calls
    [docker manifest inspect] — can take 10-15 seconds. *)

val refresh_base_digest : t -> (t, [> Rresult.R.msg ]) result
(** Resolve the digest and return an updated profile. Caller must save the
    profile. *)

val base_image_stale : ?max_age_days:int -> t -> bool
(** Returns [true] if the base image digest is older than [max_age_days]
    (default 30), or if no digest is recorded. *)

val track_limit : t -> int option
(** Convert [target_mode] into a "number of versions per package" limit for use
    with tracking pipelines (ocaml-docs-ci). [None] means no limit (track every
    version). *)

val track_filter : t -> string list
(** Convert [target_mode] into a list of package names to track. Empty list
    means no filter (track all). *)
