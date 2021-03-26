val cap : 'a Capnp_rpc_lwt.Sturdy_ref.t

val ssh_secrets : Obuilder_spec.Secret.t list

val ssh_secrets_values : (string * string) list

val ssh_host : string

val storage_folder : string

val odoc : string
(** Odoc version pin to use. *)

val odoc_bin : string
(** Local odoc binary for the final link step. Should be 
the same version as odoc *)

val pool : string
