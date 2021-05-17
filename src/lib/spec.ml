type t = { base : string; ops : Obuilder_spec.op list; children : (string * Obuilder_spec.t) list }

let add next_ops { base; ops; children } = { base; ops = ops @ next_ops; children }

let children ~name spec { base; ops; children } = { base; ops; children = (name, spec) :: children }

let finish { base; ops; children } = Obuilder_spec.stage ~child_builds:children ~from:base ops

(* https://gist.github.com/iangreenleaf/279849 *)
let rsync_retry_script =
  {|#!/bin/bash

MAX_RETRIES=10
i=0

# Set the initial return value to failure
false

while [ $? -ne 0 -a $i -lt $MAX_RETRIES ]
do
 i=$(($i+1))
 echo "Rsync ($i)"
 /usr/bin/rsync $@
done

if [ $i -eq $MAX_RETRIES ]
then
  echo "Hit maximum number of retries, giving up."
  exit 1
fi
|}

let add_rsync_retry_script =
  Obuilder_spec.run
    "echo '%s' | sudo tee -a /usr/local/bin/rsync && sudo chmod +x /usr/local/bin/rsync && which \
     rsync" rsync_retry_script

let make base =
  let open Obuilder_spec in
  {
    base;
    ops = [ user ~uid:1000 ~gid:1000; workdir "/home/opam"; run "sudo chown opam:opam /home/opam" ];
    children = [];
  }

let to_ocluster_spec build_spec =
  let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec |> finish) in
  let open Cluster_api.Obuilder_job.Spec in
  { spec = `Contents spec_str }
