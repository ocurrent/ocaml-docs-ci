module Git = Current_git

let network = Misc.network

let cache = Voodoo.cache

let not_base x =
  not
    (List.mem (OpamPackage.name_to_string x)
       [
         "base-unix";
         "base-bigarray";
         "base-threads";
         "ocaml-config";
         "ocaml";
         "ocaml-base-compiler";
       ])

let folder t =
  let universe = Package.universe t |> Package.Universe.hash in
  let opam = Package.opam t in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  Fpath.(v "prep" / "universes" / universe / name / version)

let base_folders packages =
  packages |> List.map (fun x -> folder x |> Fpath.to_string) |> String.concat " "

let universes_assoc packages =
  packages
  |> List.map (fun pkg ->
         let hash = pkg |> Package.universe |> Package.Universe.hash in
         let name = pkg |> Package.opam |> OpamPackage.name_to_string in
         name ^ ":" ^ hash)
  |> String.concat ","

let spec ~ssh ~remote_cache ~cache_key ~artifacts_digest ~voodoo ~base ~(install : Package.t)
    (prep : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let all_deps = Package.all_deps install in
  let packages_str =
    all_deps |> List.map Package.opam |> List.filter not_base |> List.map OpamPackage.to_string
    |> String.concat " "
  in

  (* Only enable dune cache for dune >= 2.1 - to remove errors like:
     #=== ERROR while compiling base64.2.3.0 =======================================#
     # context              2.0.8 | linux/x86_64 | ocaml-base-compiler.4.06.1 | file:///src
     # path                 ~/.opam/4.06/.opam-switch/build/base64.2.3.0
     # command              ~/.opam/4.06/bin/dune build -p base64 -j 127
     # exit-code            1
     # env-file             ~/.opam/log/base64-1-9efc19.env
     # output-file          ~/.opam/log/base64-1-9efc19.out
     ### output ###
     # Error: link: /home/opam/.cache/dune/db/v2/temp/promoting: Invalid cross-device link
  *)
  let dune_cache_enabled =
    all_deps
    |> List.exists (fun p ->
           let opam = Package.opam p in
           let min_dune_version = OpamPackage.Version.of_string "2.1.0" in
           match OpamPackage.name_to_string opam with
           | "dune" -> OpamPackage.Version.compare (OpamPackage.version opam) min_dune_version >= 0
           | _ -> false)
  in
  let build_preinstall =
    List.filter
      (fun pkg ->
        let name = pkg |> Package.opam |> OpamPackage.name_to_string in
        name = "dune" || name = "ocamlfind")
      all_deps
    |> function
    | [] -> comment "no build system"
    | lst ->
        let packages_str =
          lst |> List.sort Package.compare
          |> List.map (fun pkg -> Package.opam pkg |> OpamPackage.to_string)
          |> String.concat " "
        in
        run ~network ~cache "opam depext -viy %s && opam install %s" packages_str packages_str
  in

  let tools = Voodoo.Prep.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         (* Install required packages *)
         copy [ "." ] ~dst:"/src";
         run "opam repo remove default && opam repo add opam /src";
         (* Pre-install build tools *)
         build_preinstall;
         (* Enable build cache conditionally on dune version *)
         env "DUNE_CACHE" (if dune_cache_enabled then "enabled" else "disabled");
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
         (* Intall packages: this might fail.
            TODO: we could still do the prep step for the installed packages. *)
         run ~secrets:Config.Ssh.secrets ~network ~cache "sudo apt update && opam depext -viy %s"
           packages_str;
         run ~cache "du -sh /home/opam/.cache/dune";
         (* empty preps should yield an empty folder *)
         run "mkdir -p %s" (base_folders prep);
         copy ~from:(`Build "tools") [ "/home/opam/voodoo-prep" ] ~dst:"/home/opam/";
         (* Perform the prep step for all packages *)
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc prep);
         (* Upload artifacts *)
         run ~secrets:Config.Ssh.secrets ~network:Misc.network "rsync -avz prep %s:%s/ && echo '%s'"
           (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh) artifacts_digest;
         (* Compute artifacts digests, write cache key and upload them *)
         run "%s" (Remote_cache.cmd_write_key cache_key (prep |> List.rev_map folder));
         run "%s" (Remote_cache.cmd_compute_sha256 (prep |> List.rev_map folder));
         run ~secrets:Config.Ssh.secrets ~network:Misc.network "%s"
           (Remote_cache.cmd_sync_folder remote_cache);
       ]

module Prep = struct
  type t = Remote_cache.t

  let id = "voodoo-prep"

  let auto_cancel = true

  module Key = struct
    type t = {
      job : Jobs.t;
      voodoo : Voodoo.Prep.t;
      artifacts_cache : Remote_cache.cache_entry list;
      config : Config.t;
    }

    let partial_digest { job = { install; _ }; voodoo; _ } =
      Package.digest install ^ Voodoo.Prep.digest voodoo |> Digest.string |> Digest.to_hex

    let digest ({ artifacts_cache; _ } as key) =
      (List.map Remote_cache.digest artifacts_cache |> String.concat "-") ^ partial_digest key
      |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ job = { install; _ }; _ } = Fmt.pf f "Voodoo prep %a" Package.pp install

  module Value = struct
    type item = { package_digest : string; artifacts_digest : string } [@@deriving yojson]

    type t = item list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let remote_cache_key Key.{ voodoo; _ } =
    (* When this key changes, the remote artifacts will be invalidated. *)
    Fmt.str "voodoo-prep-v0-%s" (Voodoo.Prep.digest voodoo)

  let build digests job (Key.{ job = { install; prep }; voodoo; artifacts_cache; config } as key) =
    let open Lwt.Syntax in
    (* Problem: no rebuild if the opam definition changes without affecting the universe hash.
       Should be fixed by adding the oldest opam-repository commit in the universe hash, but that
       requires changes in the solver.
       For now we rebuild only if voodoo-prep changes.
    *)
    let cache_key = remote_cache_key key in
    let prev_logs_path = Fpath.(Current.state_dir id // folder install / (cache_key ^ ".log")) in
    let is_success = function
      | Some (key, Remote_cache.Ok _) when String.trim key = cache_key -> true
      | _ -> false
    in
    let is_failure = function
      | Some (key, Remote_cache.Failed) when String.trim key = cache_key -> true
      | _ -> false
    in
    if List.for_all is_success artifacts_cache then (
      (* Success: all the packages we want to prep have already been built *)
      let* () = Current.Job.start ~level:Harmless job in
      Current.Job.log job "Using existing artifacts.";
      ( match Bos.OS.File.read prev_logs_path with
      | Ok logs ->
          Current.Job.log job "Previous log output for this build:\n\n>>>";
          Current.Job.write job (logs ^ "<<<\n\n")
      | Error _ -> Current.Job.log job "Couldn't find previous logs locally." );
      List.combine prep artifacts_cache
      |> List.map (fun (package, artifacts_digest) ->
             Current.Job.log job "- %a: %s" Fpath.pp (folder package)
               (Remote_cache.folder_digest_exn artifacts_digest);
             Value.
               {
                 package_digest = Package.digest package;
                 artifacts_digest = Remote_cache.folder_digest_exn artifacts_digest;
               })
      |> Lwt.return_ok )
    else if List.exists is_failure artifacts_cache then (
      (* Failure: we already tried to build one of the artifacts. We won't bother trying again. *)
      let* () = Current.Job.start ~level:Harmless job in
      Current.Job.log job "This job already failed.";
      ( match Bos.OS.File.read prev_logs_path with
      | Ok logs ->
          Current.Job.log job "Previous log output for this build:\n\n>>>";
          Current.Job.write job (logs ^ "<<<\n\n")
      | Error _ -> Current.Job.log job "Couldn't find previous logs locally." );
      Lwt.return_error (`Msg ("Prep step failed for " ^ Fmt.to_to_string Package.pp install)) )
    else
      (* Launch a prep job *)
      let artifacts_cache = List.combine prep artifacts_cache in
      let fetch_digest =
        List.map (fun (_, x) -> Remote_cache.digest x) artifacts_cache
        |> String.concat "-" |> Digest.string |> Digest.to_hex
      in
      (* Only prep what's not been successful. *)
      let to_prep =
        List.filter_map
          (function prep, digest when not (is_success digest) -> Some prep | _ -> None)
          artifacts_cache
      in
      let base = Misc.get_base_image install in
      let Cluster_api.Obuilder_job.Spec.{ spec = `Contents spec } =
        spec ~ssh:(Config.ssh config) ~remote_cache:digests ~cache_key
          ~artifacts_digest:fetch_digest ~voodoo ~base ~install to_prep
        |> Spec.to_ocluster_spec
      in
      let action = Cluster_api.Submission.obuilder_build spec in
      let src = ("https://github.com/ocaml/opam-repository.git", [ Package.commit install ]) in
      let version = Misc.base_image_version install in
      let cache_hint = "docs-universe-prep-" ^ version in
      let build_pool =
        Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint ~src
          ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
          (Config.ocluster_connection_prep config)
      in
      let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
      Current.Job.log job "Using cache hint %S" cache_hint;
      List.iter
        (fun (prep, digest) ->
          Current.Job.log job "Current artifacts digest for folder %a: %a" Fpath.pp (folder prep)
            Remote_cache.pp digest)
        artifacts_cache;
      Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
      let* result = Current_ocluster.Connection.run_job ~job build_job in
      (* at this point we save the logs.
         TODO: error handling *)
      let log_path = Current.Job.(log_path (id job)) |> Result.get_ok in
      let target_path = prev_logs_path in
      Current.Job.log job "Saving logs (%a) into %a" Fpath.pp log_path Fpath.pp target_path;
      Bos.OS.Dir.create (Fpath.parent target_path) |> Result.get_ok |> ignore;
      Bos.OS.Path.symlink ~force:true ~target:log_path target_path |> Result.get_ok;
      (* Then, handle the result *)
      let+ () = Remote_cache.sync ~job digests in
      match result with
      | Error e -> Error e
      | Ok _ ->
          let artifacts_digest =
            List.map
              (fun x ->
                let f = folder x in
                (x, Remote_cache.get digests f))
              prep
          in
          Ok
            (List.map
               (fun (package, digest) ->
                 Current.Job.log job "New artifacts digest for folder %a: %a" Fpath.pp
                   (folder package) Remote_cache.pp digest;
                 Value.
                   {
                     package_digest = Package.digest package;
                     artifacts_digest = Remote_cache.folder_digest_exn digest;
                   })
               artifacts_digest)
end

module PrepCache = Current_cache.Make (Prep)

type t = { package : Package.t; artifacts_digest : string }

let pp f t = Package.pp f t.package

let compare a b =
  match Package.compare a.package b.package with
  | 0 -> String.compare a.artifacts_digest b.artifacts_digest
  | v -> v

module StringMap = Map.Make (String)

let combine ~(job : Jobs.t) artifacts_digests =
  let packages = job.prep in
  let artifacts_digests =
    artifacts_digests |> List.to_seq
    |> Seq.map (fun Prep.Value.{ package_digest; artifacts_digest } ->
           (package_digest, artifacts_digest))
    |> StringMap.of_seq
  in
  List.map
    (fun package ->
      let digest = StringMap.find (Package.digest package) artifacts_digests in
      { package; artifacts_digest = digest })
    packages

(** Assumption: packages are co-installable *)
let v ~config ~voodoo ~(cache : Remote_cache.t Current.t) (job : Jobs.t Current.t) =
  let open Current.Syntax in
  let* jobv = job in
  Current.component "voodoo-prep %s" (jobv.install |> Package.digest)
  |> let> voodoo = voodoo and> job = job and> cache = cache in
     let artifacts_cache =
       List.map (fun package -> Remote_cache.get cache (folder package)) job.prep
     in
     PrepCache.get cache { job; voodoo; artifacts_cache; config }
     |> Current.Primitive.map_result (Result.map (combine ~job))

let package (t : t) = t.package

let artifacts_digest (t : t) = t.artifacts_digest

let folder (t : t) = folder t.package
