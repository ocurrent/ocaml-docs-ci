(** Recursive directory tree operations. *)

val hardlink :
  source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg ]) result
(** [hardlink ~source ~target] recursively hardlinks the tree rooted at [source]
    into [target]. Falls back to {!copy} on EMLINK (too many links). Symlink
    targets in the source tree are preserved as symlinks. *)

val copy : source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg ]) result
(** [copy ~source ~target] recursively copies the tree rooted at [source] into
    [target], preserving permissions and modification times. *)

val clense :
  source:Fpath.t -> target:Fpath.t -> (unit, [> Rresult.R.msg ]) result
(** [clense ~source ~target] removes files from [target] that are identical to
    their counterparts in [source] (compared by mtime). Also removes directories
    in [target] that become empty after removal. Used after overlay builds to
    extract only new/changed files. *)
