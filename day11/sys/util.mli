(** Miscellaneous utilities. *)

val nproc : unit -> int
(** [nproc ()] returns the number of available processors. *)

val dir_size : Fpath.t -> (int, [> Rresult.R.msg ]) result
(** [dir_size path] returns the total size of all files under [path] in bytes,
    computed recursively via [Unix.lstat]. Symlinks contribute their own size
    (the length of the link path), not the size of the target they point to.
    Returns [Error] if the directory cannot be read. *)
