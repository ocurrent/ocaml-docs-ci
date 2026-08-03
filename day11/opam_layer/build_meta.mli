(** Opam build sidecar: per-layer metadata for opam package builds.

    Lives next to {!Day11_layer.Meta} as [build.json] in the layer directory.
    The presence of this file marks a layer as the output of an opam package
    build (as opposed to e.g. a doc layer, see {!Doc_meta}, or a future layer
    kind that doesn't yet exist).

    The opam-specific information is kept here so that {!Day11_layer.Meta} can
    stay generic and reusable across layer kinds. *)

type dep = {
  pkg : string;  (** Direct dependency, as a [name.version] string. *)
  hash : string;
      (** Layer hash of the dep's build. Matches the corresponding entry in
          {!Day11_layer.Meta.t.parent_hashes}. Empty when reading an older
          [build.json] that recorded deps as plain strings. *)
}

type t = {
  package : string;
      (** The opam package this layer was built for, as a [name.version] string.
      *)
  deps : dep list;
      (** Direct dependencies with their layer hashes. Order is parallel to
          {!Day11_layer.Meta.t.parent_hashes}. *)
  stack : string list;
      (** Transitive dependency layer hashes in overlayfs-stack order — the
          exact lower stack this build ran over (direct deps frontmost /
          topmost). A superset of {!deps}, which lists only direct dependencies;
          recording the full ordered closure lets a tool reconstruct the rootfs
          without walking and re-ordering the dependency DAG itself. Empty in
          older [build.json] files (fall back to deriving it from the DAG). *)
  build_deps : string list;
      (** Transitive build-dependency closure as sorted ["name.version"] strings
          (excludes this package). The human/diff-friendly projection of
          {!stack} — materialised here so a reader can show a build's full dep
          set from one file without walking the layer tree. Empty in older
          [build.json] files. *)
  installed_libs : string list;
      (** Files under [/home/opam/.opam/default/lib/] that this build installed.
          Discovered by {!Installed_files.scan_libs} after the container exits.
      *)
  installed_docs : string list;
      (** Files under [/home/opam/.opam/default/doc/] that this build installed.
          Discovered by {!Installed_files.scan_docs}. *)
  patches : string list;
      (** Filenames of patch files applied to this package before building, if
          any. *)
  base_image : string;
      (** The base Docker image the build used (e.g.
          ["debian-12-ocaml-5.2:abc123"]). Persisting it lets a cold rerun
          re-import the base layer without consulting an external profile. Empty
          in older [build.json] files. *)
  cmd : string;
      (** The shell command run inside the build container, e.g.
          ["opam-build -v fmt.0.9.0 --patch /patches/000.patch"]. Empty in older
          [build.json] files. *)
  universe : string;
      (** The doc-deps universe digest this build was emitted into. Identifies
          which link-time mount stack a doc layer must use to be consistent with
          this package. Empty in older [build.json] files. *)
}

val filename : string
(** ["build.json"] — the on-disk filename relative to the layer directory. *)

val save : Fpath.t -> t -> (unit, [> Rresult.R.msg ]) result
(** [save layer_dir t] writes [t] as [layer_dir/build.json]. *)

val load : Fpath.t -> (t, [> Rresult.R.msg ]) result
(** [load layer_dir] reads [layer_dir/build.json] and parses it. Returns [Error]
    if the file is missing or malformed. *)

val exists : Fpath.t -> bool
(** [exists layer_dir] is [true] iff [layer_dir/build.json] exists. Use this to
    identify "this is an opam build layer" without needing to parse the
    contents. *)

val load_tree :
  Eio_unix.Stdenv.base ->
  os_dir:Fpath.t ->
  string ->
  (Build.t, [> Rresult.R.msg ]) result
(** [load_tree env ~os_dir hash] reconstructs a {!Build.t} tree by recursively
    reading layer.json + build.json files starting at the layer for [hash]. The
    [hash] is the full hash (not truncated). *)
