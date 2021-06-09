(* Pages - /packages/index.html and packages/<foo>/index.html *)

let id = "pages"

let spec ~ssh ~base ~voodoo () =
  let open Obuilder_spec in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  let branches = [Git_store.status_branch] in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do"; "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
        Git_store.Cluster.pull_to_directory ~repository:HtmlTailwind ~ssh
          ~directory:"html" ~branches;
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-gen packages -o html";
         Git_store.Cluster.write_folders_to_git ~repository:HtmlTailwind ~ssh
           ~branches:[Git_store.status_branch, "."] ~folder:"html" ~message:"Update pages"
           ~git_path:"/tmp/git-store";
        run "cd /tmp/git-store && %s" (Git_store.print_branches_info ~prefix:"HASHES" ~branches);
       ]

module Pages = struct
  type t = { voodoo : Voodoo.Do.t; config : Config.t }

  let id = "update-pages"

  let auto_cancel = true

  module Key = struct
    type t = string

    let digest v = Format.asprintf "pages-%s" v
  end

  module Value = struct
    type t = Current_git.Commit.t

    let digest = Fmt.to_to_string Current_git.Commit.pp
  end

  module Outcome = struct
    type t = [ `Branch of string ] * [ `Commit of string ] [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let pp fmt (_k, v) = Format.fprintf fmt "metadata-%a" Current_git.Commit.pp v

  let publish { voodoo; config } job _ _v : Outcome.t Current.or_error Lwt.t =
    Current.Job.log job "Publish pages";
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let base = Spec.make "ocaml/opam:ubuntu-ocaml-4.12" in
    let spec = spec ~ssh:(Config.ssh config) ~base ~voodoo () in
    let action = Misc.to_ocluster_submission spec in
    let version = "4.12" in
    let cache_hint = "docs-universe-compile-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_do config)
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
    match git_hashes, failed with
    | [info], [] ->
        Lwt.return_ok (`Branch "status", `Commit info.Git_store.commit_hash)
    | _ ->
      Lwt.return_error (`Msg "Odd hash return")
end

module PagesCache = Current_cache.Output (Pages)

let v ~config ~voodoo ~commit =
  let open Current.Syntax in
  Current.component "publish-pages"
  |> let> voodoo = voodoo and> commit = commit in
     PagesCache.set { config; voodoo } "pages" commit
