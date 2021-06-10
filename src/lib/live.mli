val publish :
  ssh:Config.Ssh.t ->
  repository:Git_store.repository ->
  branch:string Current.t ->
  commits:(Git_store.Branch.t * [ `Commit of string ]) list Current.t ->
  unit Current.t

val set_live_to :
  ssh:Config.Ssh.t ->
  repository:Git_store.repository ->
  branch:string Current.t ->
  message:string Current.t ->
  unit Current.t
