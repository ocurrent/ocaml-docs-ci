

module Cluster : sig
  val clone : branch:string -> directory:string -> Config.Ssh.t -> Obuilder_spec.op

  val push : ?force:bool -> Config.Ssh.t -> Obuilder_spec.op

end

val remote : Config.Ssh.t -> string
