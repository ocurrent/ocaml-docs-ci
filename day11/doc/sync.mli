(** Documentation distribution via rsync.

    Syncs generated documentation from the local cache to a remote or local
    destination. *)

type doc_entry = {
  pkg : OpamPackage.t;
  html_path : Fpath.t;
  universe : string;
  blessed : bool;
}
(** A documentation entry discovered in the cache. *)

val scan_cache : Eio_unix.Stdenv.base -> os_dir:Fpath.t -> doc_entry list
(** [scan_cache env ~os_dir] scans for doc layers and extracts their HTML output
    paths. *)

val sync :
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  entries:doc_entry list ->
  destination:string ->
  ?blessed_only:bool ->
  ?package_filter:(string -> bool) ->
  ?dry_run:bool ->
  ?on_progress:(done_count:int -> total:int -> pkg:OpamPackage.t -> unit) ->
  unit ->
  (unit, [> Rresult.R.msg ]) result
(** [sync ~sw env ~entries ~destination ?blessed_only ?package_filter ?dry_run
     ?on_progress ()] rsyncs documentation to [destination]. [on_progress] is
    invoked after each package's rsync completes (regardless of success) with
    the running count of processed entries. *)
