type t = { base : string; ops : Obuilder_spec.op list }

let add next_ops { base; ops } = { base; ops = ops @ next_ops }

let finish { base; ops } = Obuilder_spec.stage ~from:base ops

let make base =
  let open Obuilder_spec in
  {
    base;
    ops = [ user ~uid:1000 ~gid:1000; workdir "/home/opam"; run "sudo chown opam:opam /home/opam" ];
  }

let to_ocluster_spec build_spec =
  let spec_str = Fmt.to_to_string Obuilder_spec.pp (build_spec |> finish) in
  let open Cluster_api.Obuilder_job.Spec in
  { spec = `Contents spec_str }
