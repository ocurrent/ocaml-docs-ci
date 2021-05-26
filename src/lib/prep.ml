(** this module stores which packages have already been successfully prepped *)
module PrepState = struct
  type state = Success | Pending | Failed

  type t = state Package.Map.t ref

  let state = ref Package.Map.empty

  let get package = try Package.Map.find package !state with Not_found -> Pending

  let update job status =
    List.iter
      (fun v ->
        state :=
          Package.Map.update v
            (function
              | None -> Some status
              | Some Failed -> Some status
              | Some Success -> Some Success
              | Some Pending when status = Success -> Some Success
              | Some Pending -> Some Pending)
            !state)
      job.Jobs.prep
end

let prep_version = "v0"

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

let folder package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  Fpath.(v "universes" / universe / name / version)

let base_folders packages =
  packages |> List.map (fun x -> folder x |> Fpath.to_string) |> String.concat " "

let universes_assoc packages =
  packages
  |> List.map (fun pkg ->
         let hash = pkg |> Package.universe |> Package.Universe.hash in
         let name = pkg |> Package.opam |> OpamPackage.name_to_string in
         name ^ ":" ^ hash)
  |> String.concat ","

let spec ~ssh ~message ~voodoo ~base ~(install : Package.t) (prep : Package.t list) =
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
  let branches = List.map Git_store.Branch.v all_deps in

  let folders ?(prefix = "") () =
    all_deps
    |> List.map (fun package ->
           (Git_store.Branch.v package, prefix ^ (folder package |> Fpath.to_string)))
  in

  let tools = Voodoo.Prep.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         (* Install required packages *)
         run "sudo mkdir /src";
         copy [ "packages" ] ~dst:"/src/packages";
         copy [ "repo" ] ~dst:"/src/repo";
         run "opam repo remove default && opam repo add opam /src";
         (* Pre-install build tools *)
         build_preinstall;
         (* Enable build cache conditionally on dune version *)
         env "DUNE_CACHE" (if dune_cache_enabled then "enabled" else "disabled");
         env "DUNE_CACHE_TRANSPORT" "direct";
         env "DUNE_CACHE_DUPLICATION" "copy";
         (* Intall packages: this might fail.
            TODO: we could still do the prep step for the installed packages. *)
         run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
         run ~cache "du -sh /home/opam/.cache/dune";
         (* empty preps should yield an empty folder *)
         run "mkdir -p %s" (base_folders prep);
         copy ~from:(`Build "tools") [ "/home/opam/voodoo-prep" ] ~dst:"/home/opam/";
         (* Perform the prep step for all packages *)
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc prep);
         (* Extract artifacts  - cache needs to be invalidated if we want to be able to read the logs *)
         run "echo '%f'" (Random.float 1.);
         Git_store.Cluster.write_folders_to_git ~repository:Prep ~ssh ~branches:(folders ())
           ~folder:"prep" ~message ~git_path:"/tmp/git-store";
         (* Compute hashes *)
         run "cd /tmp/git-store && %s" (Git_store.print_branches_info ~prefix:"HASHES" ~branches);
       ]

module Prep = struct
  type t = No_context

  let id = "voodoo-prep"

  let auto_cancel = true

  module Key = struct
    type t = { job : Jobs.t; voodoo : Voodoo.Prep.t; config : Config.t }

    let digest { job = { install; _ }; voodoo; _ } =
      Fmt.str "%s\n%s\n%s" prep_version (Package.digest install) (Voodoo.Prep.digest voodoo)
      |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ job = { install; _ }; _ } = Fmt.pf f "Voodoo prep %a" Package.pp install

  module Value = struct
    type item = Git_store.branch_info [@@deriving yojson]

    type t = item list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job (Key.{ job = { install; prep }; voodoo; config } as key) =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    (* Problem: no rebuild if the opam definition changes without affecting the universe hash.
       Should be fixed by adding the oldest opam-repository commit in the universe hash, but that
       requires changes in the solver.
       For now we rebuild only if voodoo-prep changes.
    *)
    (* Only prep what's not been successful. *)
    let to_prep =
      List.filter_map
        (function prep when PrepState.get prep <> PrepState.Success -> Some prep | _ -> None)
        prep
    in
    let base = Misc.get_base_image install in
    let message = Fmt.str "Update\n\n%s" (Key.digest key) in
    let spec = spec ~ssh:(Config.ssh config) ~message ~voodoo ~base ~install to_prep in
    let action = Misc.to_ocluster_submission spec in
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
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let** _ = Current_ocluster.Connection.run_job ~job build_job in
    (* extract result from logs *)
    let rec aux start next_lines accumulator =
      match next_lines with
      | [] -> (
          let* logs = Cluster_api.Job.log build_job start in
          match logs with
          | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
          | Ok ("", _) -> Lwt_result.return accumulator
          | Ok (data, next) ->
              let lines = String.split_on_char '\n' data in
              aux next lines accumulator )
      | line :: next ->
          aux start next (Git_store.parse_branch_info ~prefix:"HASHES" line :: accumulator)
    in
    aux 0L [] []
    |> Lwt_result.map
         (List.filter_map (fun line ->
              Option.iter
                (fun (r : Git_store.branch_info) ->
                  Current.Job.log job "%s -> commit %s / tree %s" r.branch r.commit_hash r.tree_hash)
                line;
              line))
end

module PrepCache = Current_cache.Make (Prep)

type t = { commit_hash : string; tree_hash : string; package : Package.t }

let commit_hash t = t.commit_hash

let tree_hash t = t.tree_hash

let package t = t.package

let pp f t = Fmt.pf f "%s:%s" t.commit_hash t.tree_hash

let compare a b =
  match String.compare a.commit_hash b.commit_hash with
  | 0 -> String.compare a.tree_hash b.tree_hash
  | v -> v

module StringMap = Map.Make (String)

let combine ~(job : Jobs.t) artifacts_branches_output =
  let packages = job.prep in
  let artifacts_branches_output =
    artifacts_branches_output |> List.to_seq
    |> Seq.map (fun Git_store.{ branch; commit_hash; tree_hash } ->
           (branch, (commit_hash, tree_hash)))
    |> StringMap.of_seq
  in
  packages |> List.to_seq
  |> Seq.map (fun package ->
         ( package,
           let package_branch = Git_store.branch_of_package package in
           let commit_hash, tree_hash = StringMap.find package_branch artifacts_branches_output in
           { package; commit_hash; tree_hash } ))
  |> Package.Map.of_seq

(** Assumption: packages are co-installable *)
let v ~config ~voodoo (job : Jobs.t) =
  let open Current.Syntax in
  Current.component "voodoo-prep %s" (job.install |> Package.digest)
  |> let> voodoo = voodoo in
     PrepCache.get No_context { job; voodoo; config }
     |> Current.Primitive.map_result (Result.map (combine ~job))
