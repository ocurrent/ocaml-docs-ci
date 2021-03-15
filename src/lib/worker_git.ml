
let ssh_config =
  {|Host ci.mirage.io
    IdentityFile ~/.ssh/id_rsa
    Port 10022
    User git
    StrictHostKeyChecking=no
|}

open Obuilder_spec

let ops = [
  run "echo '%s' >> .ssh/id_rsa && chmod 600 .ssh/id_rsa" Key.priv;
  run "echo '%s' >> .ssh/id_rsa.pub" Key.pub;
  run "echo '%s' >> .ssh/config" ssh_config;
]