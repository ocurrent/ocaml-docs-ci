type ssh = {
  host : string;
  user : string;
  port : int;
  private_key_file : string;
  public_key_file : string;
  folder : string;
}
[@@deriving yojson]

type config = {
  (* Capability file for ocluster submissions *)
  cap_file : string;
  ssh_storage : ssh;
}
[@@deriving yojson]

let v = Yojson.Safe.from_file "config.json" |> config_of_yojson |> Result.get_ok

let vat = Capnp_rpc_unix.client_only_vat ()

let cap = Capnp_rpc_unix.Cap_file.load vat v.cap_file |> Result.get_ok

let odoc = "https://github.com/ocaml/odoc.git#50fcb86ae66bb7d223b0d5e90488c7a911d22541"

let storage_folder = v.ssh_storage.folder

let ssh_host = v.ssh_storage.host

let ssh_config =
  Fmt.str
    {|Host %s
    IdentityFile ~/.ssh/id_rsa
    Port %d
    User %s
    StrictHostKeyChecking=no
  |}
    v.ssh_storage.host v.ssh_storage.port v.ssh_storage.user

let ssh_secrets =
  Obuilder_spec.Secret.
    [
      v ~target:"/home/opam/.ssh/id_rsa" "ssh_privkey";
      v ~target:"/home/opam/.ssh/id_rsa.pub" "ssh_pubkey";
      v ~target:"/home/opam/.ssh/config" "ssh_config";
    ]

let load_file path =
  try
    let ch = open_in path in
    let len = in_channel_length ch in
    let data = really_input_string ch len in
    close_in ch;
    data
  with ex ->
    if Sys.file_exists path then failwith @@ Fmt.str "Error loading %S: %a" path Fmt.exn ex
    else failwith @@ Fmt.str "File %S does not exist" path

let ssh_privkey = load_file v.ssh_storage.private_key_file

let ssh_pubkey = load_file v.ssh_storage.public_key_file

let ssh_secrets_values = [ ("ssh_privkey", ssh_privkey); ("ssh_pubkey", ssh_pubkey); ("ssh_config", ssh_config) ]
