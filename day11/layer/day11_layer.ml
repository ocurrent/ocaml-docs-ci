(** Content-addressed layer storage.

    A layer is a directory on disk identified by its content hash. It contains:
    - [fs/] — the filesystem tree (overlayfs upper)
    - [layer.json] — metadata ({!Meta.t})
    - [layer.log] — build stdout/stderr
    - optional sidecar files ([build.json], [doc.json], etc.) *)

(** {1 Core type} *)

type t = Layer.t = { hash : string; dir : Fpath.t }

include (Layer : module type of Layer with type t := t)

(** {1 Modules} *)

module Base = Base
module Dir = Dir
module Hash = Hash
module Import = Import
module Last_used = Last_used
module Layer = Layer
module Layer_status = Layer_status
module Meta = Meta
module Relocations = Relocations
module Scan = Scan
module Snapshot = Snapshot
module Stack = Stack
module Symlinks = Symlinks
