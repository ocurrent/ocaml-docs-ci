(* Pages - /packages/index.html and packages/<foo>/index.html *)

let id = "pages"

let spec ~ssh ~base ~voodoo ~metadata_branch () =
  let open Obuilder_spec in
  let metadata_branch, commit = metadata_branch in
  let tools = Voodoo.Gen.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         copy ~from:(`Build "tools")
           [ "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
         Git_store.Cluster.pull_to_directory ~repository:HtmlTailwind ~ssh ~directory:"html"
           ~branches:[ (metadata_branch, commit) ];
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-gen packages -o html";
         Git_store.Cluster.write_folders_to_git ~repository:HtmlTailwind ~ssh
           ~branches:[ (Git_store.Branch.status, ".") ]
           ~folder:"html" ~message:"Update pages" ~git_path:"/tmp/git-store";
         run "cd /tmp/git-store && %s"
           (Git_store.print_branches_info ~prefix:"HASHES" ~branches:[ Git_store.Branch.status ]);
       ]

module Pages = struct
  type t = { voodoo : Voodoo.Gen.t; config : Config.t }

  let id = "update-pages"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest v = Format.asprintf "pages-%s" v
  end

  module Value = struct
    type t = Git_store.Branch.t * [ `Commit of string ] [@@deriving yojson]

    let digest (v, `Commit c) = Git_store.Branch.to_string v ^ "@" ^ c
  end

  module Outcome = struct
    type t = Git_store.Branch.t * [ `Commit of string ] [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let pp fmt (_k, (_, `Commit v)) = Format.fprintf fmt "metadata: %s"  v

  let publish { voodoo; config } job _ metadata_branch : Outcome.t Current.or_error Lwt.t =
    Current.Job.log job "Publish pages";
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
    let spec = spec ~ssh:(Config.ssh config) ~base ~voodoo ~metadata_branch () in
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
      match Git_store.parse_branch_info ~prefix:"HASHES" line with
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
    | [ info ], [] -> Lwt.return_ok (Git_store.Branch.status, `Commit info.Git_store.commit_hash)
    | _ -> Lwt.return_error (`Msg "Odd hash return")
end

module PagesCache = Current_cache.Output (Pages)

let v ~config ~voodoo ~metadata_branch =
  let open Current.Syntax in
  Current.component "publish-pages"
  |> let> voodoo = voodoo and> metadata_branch = metadata_branch in
     PagesCache.set { config; voodoo } "pages" metadata_branch
