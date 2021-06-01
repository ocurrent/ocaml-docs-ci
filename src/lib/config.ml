open Cmdliner

module Ssh = struct
  type t = {
    host : string;
    user : string;
    port : int;
    private_key : string;
    private_key_file : string;
    public_key : string;
    folder : string;
    html_endpoint : string;
  }

  let named f = Cmdliner.Term.(app (const f))

  let ssh_host =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage server host" ~docv:"HOST" [ "ssh-host" ]
    |> named (fun x -> `SSH_host x)

  let ssh_user =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage server user" ~docv:"USER" [ "ssh-user" ]
    |> named (fun x -> `SSH_user x)

  let ssh_port =
    Arg.required
    @@ Arg.opt Arg.(some int) (Some 22)
    @@ Arg.info ~doc:"SSH storage server port" ~docv:"PORT" [ "ssh-port" ]
    |> named (fun x -> `SSH_port x)

  let ssh_privkey =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH private key file" ~docv:"FILE" [ "ssh-privkey" ]
    |> named (fun x -> `SSH_privkey x)

  let ssh_pubkey =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH public key file" ~docv:"FILE" [ "ssh-pubkey" ]
    |> named (fun x -> `SSH_pubkey x)

  let ssh_folder =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH storage folder" ~docv:"FILE" [ "ssh-folder" ]
    |> named (fun x -> `SSH_folder x)

  let ssh_endpoint =
    Arg.required
    @@ Arg.opt Arg.(some string) None
    @@ Arg.info ~doc:"SSH <storage folder>/html/ public endpoint" ~docv:"FILE" [ "ssh-endpoint" ]
    |> named (fun x -> `SSH_endpoint x)

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

  let v (`SSH_host host) (`SSH_user user) (`SSH_port port) (`SSH_pubkey pubkey)
      (`SSH_privkey privkey) (`SSH_folder folder) (`SSH_endpoint endpoint) =
    {
      host;
      user;
      port;
      private_key = load_file privkey;
      private_key_file =
        Fpath.(
          (Bos.OS.Dir.current () |> Result.get_ok) // (of_string privkey |> Result.get_ok)
          |> to_string);
      public_key = load_file pubkey;
      folder;
      html_endpoint = endpoint;
    }

  let cmdliner =
    Term.(
      const v $ ssh_host $ ssh_user $ ssh_port $ ssh_pubkey $ ssh_privkey $ ssh_folder
      $ ssh_endpoint)

  let config t =
    Fmt.str
      {|Host %s
          IdentityFile ~/.ssh/id_rsa
          Port %d
          User %s
          StrictHostKeyChecking=no
          GlobalKnownHostsFile=/dev/null
          UserKnownHostsFile=/dev/null
          ConnectTimeout=10
        |}
      t.host t.port t.user

  let secrets =
    Obuilder_spec.Secret.
      [
        v ~target:"/home/opam/.ssh/id_rsa" "ssh_privkey";
        v ~target:"/home/opam/.ssh/id_rsa.pub" "ssh_pubkey";
        v ~target:"/home/opam/.ssh/config" "ssh_config";
      ]

  let secrets_values t =
    [ ("ssh_privkey", t.private_key); ("ssh_pubkey", t.public_key); ("ssh_config", config t) ]

  let storage_folder t = t.folder

  let host t = t.host

  let user t = t.user

  let priv_key_file t = Fpath.v t.private_key_file

  let port t = t.port

  let docs_public_endpoint t = t.html_endpoint

  let digest t =
    Fmt.str "%s-%s-%d-%s" t.host t.user t.port t.folder |> Digest.string |> Digest.to_hex
end

type t = {
  jobs : int;
  track_packages : string list;
  take_n_last_versions : int option;
  ocluster_connection_prep : Current_ocluster.Connection.t;
  ocluster_connection_do : Current_ocluster.Connection.t;
  ssh : Ssh.t;
}

let cap_file =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Ocluster capability file" ~docv:"FILE" [ "ocluster-submission" ]

let jobs =
  Arg.required
  @@ Arg.opt Arg.(some int) (Some 8)
  @@ Arg.info ~doc:"Number of parallel jobs on the host machine (for solver)" ~docv:"JOBS"
       [ "jobs"; "j" ]

let track_packages =
  Arg.value
  @@ Arg.opt Arg.(list string) []
  @@ Arg.info ~doc:"Filter the name of packages to track. " ~docv:"PKGS" [ "filter" ]

let take_n_last_versions =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info ~doc:"Limit the number of versions" ~docv:"LIMIT" [ "limit" ]

let v cap_file jobs track_packages take_n_last_versions ssh =
  let vat = Capnp_rpc_unix.client_only_vat () in
  let cap = Capnp_rpc_unix.Cap_file.load vat cap_file |> Result.get_ok in

  let ocluster_connection_prep = Current_ocluster.Connection.create ~max_pipeline:100 cap in
  let ocluster_connection_do = Current_ocluster.Connection.create ~max_pipeline:100 cap in

  {
    jobs;
    track_packages;
    take_n_last_versions;
    ocluster_connection_prep;
    ocluster_connection_do;
    ssh;
  }

let cmdliner =
  Term.(const v $ cap_file $ jobs $ track_packages $ take_n_last_versions $ Ssh.cmdliner)

let odoc _ = "https://github.com/ocaml/odoc.git#7dec59ed822de03dd54dafe1a5c2e51ee63619de"

let pool _ = "linux-x86_64"

let jobs t = t.jobs

let track_packages t = t.track_packages

let take_n_last_versions t = t.take_n_last_versions

let ocluster_connection_do t = t.ocluster_connection_do

let ocluster_connection_prep t = t.ocluster_connection_prep

let ssh t = t.ssh
