(* Pages - /packages/index.html and packages/<foo>/index.html *)

let id = "pages"

let spec ~ssh ~generation ~base ~voodoo ~input_hash () =
  let open Obuilder_spec in
  let tools = Voodoo.Gen.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         copy ~from:(`Build "tools") [ "/home/opam/voodoo-gen" ] ~dst:"/home/opam/";
         (* we want to obtain /g-<id>/html-tailwind/packages/<name>/package.json files *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets
           "rsync -avzR --exclude=\"/*/*/*/*/*/\" %s:%s/./%s . # %s" (Config.Ssh.host ssh)
           (Config.Ssh.storage_folder ssh)
           (Fpath.to_string (Storage.Base.folder (HtmlTailwind generation))) input_hash;
         run "OCAMLRUNPARAM=b cd %s && opam exec -- /home/opam/voodoo-gen packages -o html-tailwind"
           (Fpath.to_string (Storage.Base.generation_folder generation));
         run ~network:Misc.network ~secrets:Config.Ssh.secrets
           "rsync -avzR --exclude=\"/*/*/*/*/*/\" ./%s %s:%s/.  "
           (Fpath.to_string (Storage.Base.folder (HtmlTailwind generation)))
           (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
         run
           "HASH=$(find . -maxdepth 3 -type f -name '*.html' -exec sha256sum {} \\; | sort | \
            sha256sum); printf \"HASH:pages:$HASH\n\
            \"";
       ]

module Pages = struct
  type t = { voodoo : Voodoo.Gen.t; config : Config.t; generation : Epoch.t }

  let id = "update-pages"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest v = Format.asprintf "pages-%s" v
  end

  module Value = struct
    type t = string

    let digest t = t
  end

  module Outcome = struct
    type t = string

    let marshal t = t

    let unmarshal t = t
  end

  let pp fmt (_k, h) = Format.fprintf fmt "metadata: %s" h

  let publish { voodoo; config; generation } job _ input_hash : Outcome.t Current.or_error Lwt.t =
    Current.Job.log job "Publish pages";
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
    let spec = spec ~ssh:(Config.ssh config) ~base ~voodoo ~generation ~input_hash () in
    let action = Misc.to_ocluster_submission spec in
    let version = "4.12" in
    let cache_hint = "docs-universe-compile-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_gen config)
    in
    let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let** _ = Current_ocluster.Connection.run_job ~job build_job in
    let extract_hashes (git_hashes, failed) line =
      match Storage.parse_hash ~prefix:"HASH" line with
      | Some value -> (value :: git_hashes, failed)
      | None -> (
          match String.split_on_char ':' line with
          | [ prev; branch ] when Astring.String.is_suffix ~affix:"FAILED" prev ->
              Current.Job.log job "Failed: %s" branch;
              (git_hashes, branch :: failed)
          | _ -> (git_hashes, failed) )
    in

    let** git_hashes, failed = Misc.fold_logs build_job extract_hashes ([], []) in
    match (git_hashes, failed) with
    | [ info ], [] -> Lwt.return_ok info.hash
    | _ -> Lwt.return_error (`Msg "Odd hash return")
end

module PagesCache = Current_cache.Output (Pages)

let v ~config ~generation ~voodoo ~metadata_branch =
  let open Current.Syntax in
  Current.component "publish-pages"
  |> let> voodoo = voodoo and> metadata_branch = metadata_branch and> generation = generation in
     PagesCache.set { config; voodoo; generation } "pages" metadata_branch
