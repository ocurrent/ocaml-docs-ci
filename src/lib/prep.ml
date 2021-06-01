module Git = Current_git

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

let spec ~ssh ~remote_cache ~cache_key ~voodoo ~base ~(install : Package.t)
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

  let pp_cmd_compute_digests =
    let pp_compute_digest f branch =
      (*output: "<branch>:<commit>;<tree hash>" *)
      Fmt.pf f
        {|printf \"%s:$(git rev-parse %s);$(git cat-file -p %s | grep tree | cut -f2- -d' ')\"|}
        branch branch branch
    in
    Fmt.(list ~sep:(any " && ") pp_compute_digest)
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
         run ~network ~cache "sudo apt update && opam depext -viy %s" packages_str;
         run ~cache "du -sh /home/opam/.cache/dune";
         (* empty preps should yield an empty folder *)
         run "mkdir -p %s" (base_folders prep);
         copy ~from:(`Build "tools") [ "/home/opam/voodoo-prep" ] ~dst:"/home/opam/";
         (* Perform the prep step for all packages *)
         run "opam exec -- ~/voodoo-prep -u %s" (universes_assoc prep);
         (* Upload artifacts *)
         Spec.add_rsync_retry_script;
         run "echo '%s'" cache_key;
         run ~secrets:Config.Ssh.secrets ~network "rsync -avz prep %s:%s/"
           (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
         (* Compute artifacts digests *)
         run "%s"
           (Fmt.to_to_string pp_cmd_compute_digests
              (prep |> List.rev_map Git_store.branch_of_package));
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
    type item = { package_digest : string; artifacts_digest : string } [@@deriving yojson]

    type t = item list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end


  let build digests job (Key.{ job = { install; prep }; voodoo; config } as key) =
    let open Lwt.Syntax in
    let (let**) = Lwt_result.bind in
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
    let spec =
      spec ~ssh:(Config.ssh config) ~remote_cache:digests ~cache_key:(Key.digest key)
        ~voodoo ~base ~install to_prep
    in
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
    let** result = Current_ocluster.Connection.run_job ~job build_job in
    (* Then, handle the result *)
    failwith ("result: "^result)
    
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
  packages |> List.to_seq
  |> Seq.map (fun package ->
         ( package,
           let digest = StringMap.find (Package.digest package) artifacts_digests in
           { package; artifacts_digest = digest } ))
  |> Package.Map.of_seq

(** Assumption: packages are co-installable *)
let v ~config ~voodoo (job : Jobs.t) =
  let open Current.Syntax in
  Current.component "voodoo-prep %s" (job.install |> Package.digest)
  |> let> voodoo = voodoo in
     PrepCache.get No_context { job; voodoo; config }
     |> Current.Primitive.map_result (Result.map (combine ~job))

let package (t : t) = t.package

let artifacts_digest (t : t) = t.artifacts_digest

let folder (t : t) = folder t.package
