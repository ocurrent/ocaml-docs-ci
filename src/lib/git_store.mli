

module Cluster : sig
  val clone : branch:string -> directory:string -> Config.Ssh.t -> Obuilder_spec.op

  val push : ?force:bool -> Config.Ssh.t -> Obuilder_spec.op

end

module Local : sig 
  val clone : branch:string -> directory:Fpath.t -> Config.Ssh.t -> Bos.Cmd.t

  val push : directory:Fpath.t -> Config.Ssh.t -> Bos.Cmd.t

end

val remote : Config.Ssh.t -> string
