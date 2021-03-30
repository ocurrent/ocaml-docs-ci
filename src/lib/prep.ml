module Git = Current_git

let network = Voodoo.network

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

let spec ~artifacts_digest ~voodoo ~base ~(install : Package.t) (prep : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let all_deps = Package.all_deps install in
  let packages_str =
    all_deps |> List.map Package.opam |> List.filter not_base |> List.map OpamPackage.to_string
    |> String.concat " "
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
        run ~network ~cache "opam install %s"
          ( lst |> List.sort Package.compare
          |> List.map (fun pkg -> Package.opam pkg |> OpamPackage.to_string)
          |> String.concat " " )
  in
  let tools = Voodoo.spec ~base Prep voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         (* Install required packages *)
         copy [ "." ] ~dst:"/src";
         run "opam repo remove default && opam repo add opam /src";
         (* Pre-install build tools *)
         build_preinstall;
         env "DUNE_CACHE" "enabled";
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
         run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
         run ~cache "du -sh /home/opam/.cache/dune";
         (* empty preps should yield an empty folder *)
         run "mkdir -p %s" (base_folders prep);
         copy ~from:(`Build "tools") [ "/home/opam/voodoo-prep" ] ~dst:"/home/opam/";
         (* Perform the prep step for all packages *)
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc prep);
         (* Upload artifacts *)
         run ~secrets:Config.ssh_secrets ~network:Voodoo.network
           "rsync -avz prep %s:%s/ && echo '%s'" Config.ssh_host Config.storage_folder
           artifacts_digest;
         run "%s" (Folder_digest.compute_cmd (prep |> List.rev_map folder));
         run ~secrets:Config.ssh_secrets ~network:Voodoo.network "rsync -avz digests %s:%s/"
           Config.ssh_host Config.storage_folder;
       ]

module Prep = struct
  type t = No_context

  let id = "voodoo-prep"

  let auto_cancel = true

  module Key = struct
    type t = { job : Jobs.t; voodoo : Voodoo.t; artifacts_digests : string option list }

    let digest { job = { install; _ }; voodoo; artifacts_digests } =
      (List.map (Option.value ~default:"<empty>") artifacts_digests |> String.concat "-")
      ^ Package.digest install ^ Git.Commit.hash voodoo
      |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ job = { install; _ }; _ } = Fmt.pf f "Voodoo prep %a" Package.pp install

  module Value = struct
    type item = { package_digest : string; artifacts_digest : string } [@@deriving yojson]

    type t = item list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job Key.{ job = { install; prep }; voodoo; artifacts_digests } =
    let open Lwt.Syntax in
    (* TODO: invalidation when the prep output is supposed to change  *)
    if List.for_all Option.is_some artifacts_digests then (
      let* () = Current.Job.start ~level:Harmless job in
      Current.Job.log job "Using existing artifacts.";
      List.combine prep artifacts_digests
      |> List.map (fun (package, artifacts_digest) ->
             Current.Job.log job "- %a: %s" Fpath.pp (folder package) (Option.get artifacts_digest);
             Value.
               {
                 package_digest = Package.digest package;
                 artifacts_digest = Option.get artifacts_digest;
               })
      |> Lwt.return_ok )
    else
      let artifacts_digests = List.combine prep artifacts_digests in
      let digest =
        List.map (fun (_, x) -> Option.value ~default:"<empty>" x) artifacts_digests
        |> String.concat "-" |> Digest.string |> Digest.to_hex
      in
      let to_prep =
        List.filter_map
          (fun (prep, digest) -> if Option.is_none digest then Some prep else None)
          artifacts_digests
      in
      let base = Misc.get_base_image install in
      let Cluster_api.Obuilder_job.Spec.{ spec = `Contents spec } =
        spec ~artifacts_digest:digest ~voodoo ~base ~install to_prep |> Spec.to_ocluster_spec
      in
      let action = Cluster_api.Submission.obuilder_build spec in
      let src = ("https://github.com/ocaml/opam-repository.git", [ Package.commit install ]) in
      let version = Misc.base_image_version install in
      let cache_hint = "docs-universe-prep-" ^ version in
      let build_pool =
        Current_ocluster.Connection.pool ~job ~pool:Config.pool ~action ~cache_hint ~src
          ~secrets:Config.ssh_secrets_values Config.ocluster_connection
      in
      let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
      Current.Job.log job "Using cache hint %S" cache_hint;
      List.iter
        (fun (prep, digest) ->
          Current.Job.log job "Current artifacts digest for folder %a: %s" Fpath.pp (folder prep)
            (Option.value ~default:"<empty>" digest))
        artifacts_digests;
      Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
      let* result = Current_ocluster.Connection.run_job ~job build_job in
      match result with
      | Error (`Msg _) as e -> Lwt.return e
      | Ok _ ->
          let+ () = Folder_digest.sync ~job () in
          let artifacts_digest =
            prep
            |> List.map (fun x ->
                   let f = folder x in
                   (x, Folder_digest.get () f |> Option.get))
          in
          Ok
            (List.map
               (fun (package, digest) ->
                 Current.Job.log job "New artifacts digest for folder %a: %s" Fpath.pp
                   (folder package) digest;
                 Value.{ package_digest = Package.digest package; artifacts_digest = digest })
               artifacts_digest)
end

module PrepCache = Current_cache.Make (Prep)

type t = { package : Package.t; artifacts_digest : string }

module StringMap = Map.Make (String)

(** Assumption: packages are co-installable *)
let v ~voodoo ~(digests : Folder_digest.t Current.t) (job : Jobs.t Current.t) =
  let open Current.Syntax in
  Current.component "voodoo-prep"
  |> let> voodoo = voodoo and> job = job and> digests = digests in
     let artifacts_digests =
       List.map (fun package -> Folder_digest.get digests (folder package)) job.prep
     in
     PrepCache.get No_context { job; voodoo; artifacts_digests }
     |> Current.Primitive.map_result (function
          | Error _ as e -> e
          | Ok artifacts_digests ->
              let packages = job.prep in
              let artifacts_digests =
                artifacts_digests |> List.to_seq
                |> Seq.map (fun Prep.Value.{ package_digest; artifacts_digest } ->
                       (package_digest, artifacts_digest))
                |> StringMap.of_seq
              in
              let result =
                List.map
                  (fun package ->
                    let digest = StringMap.find (Package.digest package) artifacts_digests in
                    { package; artifacts_digest = digest })
                  packages
              in
              Ok result)

let package (t : t) = t.package

let artifacts_digest (t : t) = t.artifacts_digest

let folder (t : t) = folder t.package
