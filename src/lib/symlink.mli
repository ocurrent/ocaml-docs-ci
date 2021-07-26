(** 
  Create a symbolic link on the [ssh] remove with [name] pointing to [target].
  The command is `ln -sfT [target] [name]`.
  By default it's considered as a dangerous operation.
*)

val remote_symbolic_link :
  ?level:Current.Level.t ->
  ssh:Config.Ssh.t ->
  target:Fpath.t ->
  name:Fpath.t ->
  unit ->
  unit Current.Primitive.t
