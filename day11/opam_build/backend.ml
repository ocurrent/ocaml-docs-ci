module type S = sig
  val build :
    sw:Eio.Switch.t ->
    Eio_unix.Stdenv.base ->
    Types.build_env ->
    opam_repositories:Fpath.t list ->
    ?mounts:Day11_container.Mount.t list ->
    ?patches:Patches.t ->
    ?build_dirs:Fpath.t list ->
    ?prep_upper:(upper:Fpath.t -> lowers:Fpath.t list -> unit) ->
    ?strategy:Types.build_strategy ->
    Day11_opam_layer.Build.t ->
    target_fs:Fpath.t ->
    unit ->
    (Day11_sys.Run.t * Day11_layer.Meta.timing, [> Rresult.R.msg ]) result
end
