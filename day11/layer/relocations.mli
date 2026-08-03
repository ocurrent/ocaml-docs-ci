(** Scan a layer's [fs/] tree for non-relocatable path references.

    Layers are portable between backends and machines only if the files inside
    them do not embed absolute paths that are specific to the build environment
    (e.g. [/tmp/day11_native_ABC/_opam/...]).

    {!scan} walks a layer's [fs/] and checks every regular file for occurrences
    of any [forbidden] substring. Binary files (detected by NUL in the first few
    KB) are searched byte-wise; text files are searched line by line. Results
    are stored as a [relocations.json] sidecar.

    Empty relocations = portable. A non-empty list means the layer carries
    build-time paths that a consumer won't be able to resolve. *)

val scan :
  Eio_unix.Stdenv.base ->
  layer_fs:Fpath.t ->
  forbidden:string list ->
  (string * string list) list
(** [scan env ~layer_fs ~forbidden] walks [layer_fs] and returns, for each file
    that contains at least one [forbidden] substring, a pair
    [(rel_path, occurrences)]. [occurrences] is a flat list of the forbidden
    strings found in the file (possibly with duplicates if a single forbidden
    string appears more than once).

    Skips symlinks, sockets, pipes, devices. *)

val save :
  Eio_unix.Stdenv.base ->
  Fpath.t ->
  (string * string list) list ->
  (unit, [> Rresult.R.msg ]) result
(** [save env path t] writes [t] as JSON to [path]. *)

val load : Eio_unix.Stdenv.base -> Fpath.t -> (string * string list) list option
(** [load env path] reads and parses a [relocations.json]. Returns [None] if the
    file doesn't exist or can't be parsed. *)
