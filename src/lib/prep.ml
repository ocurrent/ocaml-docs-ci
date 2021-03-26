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

let spec ~artifacts_digest ~voodoo ~base (packages : Package.t list) =
  let open Obuilder_spec in
  (* the list of packages to install *)
  let packages_str =
    packages |> List.map Package.opam |> List.filter not_base |> List.map OpamPackage.to_string
    |> String.concat " "
  in
  let tools = Voodoo.spec ~base Prep voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         (* Install required packages *)
         copy [ "." ] ~dst:"/src";
         run "opam repo remove default && opam repo add opam /src";
         env "DUNE_CACHE" "enabled";
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
         run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
         run ~cache "du -sh /home/opam/.cache/dune";
         run "mkdir -p %s" (base_folders packages);
         (* empty preps should yield an empty folder *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-prep" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc packages);
         (* Perform the prep step for all packages *)
         run ~secrets:Config.ssh_secrets ~network:Voodoo.network
           "rsync -avz prep %s:%s/ && echo '%s'" Config.ssh_host Config.storage_folder
           artifacts_digest;
       ]

module Prep = struct
  type t = No_context

  let id = "voodoo-prep"

  let auto_cancel = true

  module Key = struct
    type t = { package : Package.t; voodoo : Voodoo.t }

    let digest { package; voodoo } = Package.digest package ^ Git.Commit.hash voodoo
  end

  let pp f Key.{ package; _ } = Fmt.pf f "Voodoo prep %a" Package.pp package

  module Value = struct
    type item = { package : Package.t; artifacts_digest : string } [@@deriving yojson]

    type t = item list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job Key.{ package; voodoo } =
    let open Lwt.Syntax in
    let switch = Current.Switch.create ~label:"prep cluster build" () in
    let* () = Current.Job.start ~level:Mostly_harmless job in

    let* artifacts_digest = Misc.remote_digest ~job (folder package) in
    let artifacts_digest = Result.value artifacts_digest ~default:"an error occured" in

    Current.Job.log job "Current artifacts digest for folder %a: %s" Fpath.pp (folder package)
      artifacts_digest;

    let base = Misc.get_base_image package in
    let Cluster_api.Obuilder_job.Spec.{ spec = `Contents spec } =
      spec ~artifacts_digest ~voodoo ~base (Package.all_deps package) |> Spec.to_ocluster_spec
    in
    let action = Cluster_api.Submission.obuilder_build spec in
    let src = ("https://github.com/ocaml/opam-repository.git", [ Package.commit package ]) in
    let version = Misc.base_image_version package in
    let cache_hint = "docs-universe-prep-" ^ version in
    Current.Job.log job "Using cache hint %S" cache_hint;

    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:Config.pool ~action ~cache_hint ~src
        ~secrets:Config.ssh_secrets_values Config.ocluster_connection
    in
    let* build_job = Current.Job.use_pool ~switch job build_pool in
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let* result = Current_ocluster.Connection.run_job ~job build_job in
    let* () = Current.Switch.turn_off switch in

    let+ artifacts_digest = Misc.remote_digest ~job (folder package) in

    match (artifacts_digest, result) with
    | Error (`Msg digest_error), Error (`Msg ocluster_error) ->
        Error (`Msg (digest_error ^ "\n" ^ ocluster_error))
    | Error (`Msg digest_error), _ -> Error (`Msg digest_error)
    | _, Error (`Msg ocluster_error) -> Error (`Msg ocluster_error)
    | Ok artifacts_digest, Ok _ ->
        Current.Job.log job "New artifacts digest => %s" artifacts_digest;
        Ok
          (List.map (fun package -> Value.{ package; artifacts_digest }) (Package.all_deps package))
end

module PrepCache = Current_cache.Make (Prep)

type t = Prep.Value.item

let package (t : t) = t.package

let artifacts_digest (t : t) = t.artifacts_digest

let folder (t : t) = folder t.package

(** Assumption: packages are co-installable *)
let v ~voodoo (package : Package.t Current.t) =
  let open Current.Syntax in
  Current.component "voodoo-prep"
  |> let> voodoo = voodoo and> package = package in
     PrepCache.get No_context { package; voodoo }
