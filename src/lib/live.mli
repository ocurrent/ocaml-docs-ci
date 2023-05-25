val set_to :
  ssh:Config.Ssh.t ->
  string ->
  [ `Html | `Linked ] ->
  Epoch.t Current.t ->
  unit Current.t
