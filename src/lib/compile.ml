type t = { package : Package.t; blessed : bool; odoc : Mld.Gen.odoc_dyn; artifacts_digest : string }

let digest t =
  Package.digest t.package ^ Bool.to_string t.blessed ^ Mld.Gen.digest t.odoc ^ t.artifacts_digest

let artifacts_digest t = t.artifacts_digest

let is_blessed t = t.blessed

let odoc t = t.odoc

let package t = t.package

let network = Voodoo.network

let folder ~blessed package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fpath.(v "compile" / "packages" / name / version)
  else Fpath.(v "compile" / "universes" / universe / name / version)

let import_deps t =
  let folders = List.map (fun { package; blessed; _ } -> folder ~blessed package) t in
  Misc.rsync_pull folders

let spec ~artifacts_digest ~base ~voodoo ~deps ~blessed prep =
  let open Obuilder_spec in
  let prep_folder = Prep.folder prep in
  let package = Prep.package prep in
  let compile_folder = folder ~blessed package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let tools = Voodoo.spec ~base Do voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         import_deps deps;
         Misc.rsync_pull ~digest:(Prep.artifacts_digest prep) [ prep_folder ];
         run "find . -type d";
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
         run
           "rm -f compile/packages.mld compile/page-packages.odoc compile/packages/*.mld \
            compile/packages/*.odoc";
         run "rm -f compile/packages/%s/*.odoc" name;
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do -p %s %s" name
           (if blessed then "-b" else "");
         run "mkdir -p html";
         run ~secrets:Config.ssh_secrets ~network
           "rsync -avzR /home/opam/docs/./compile/ %s:%s/ && echo '%s'" Config.ssh_host
           Config.storage_folder artifacts_digest;
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./html/ %s:%s/"
           Config.ssh_host Config.storage_folder;
         run "%s" (Folder_digest.compute [ compile_folder ]);
         run ~secrets:Config.ssh_secrets ~network:Voodoo.network "rsync -avz digests %s:%s/"
           Config.ssh_host Config.storage_folder;
       ]

module Compile = struct
  type output = t

  type t = No_context

  let id = "voodoo-do"

  module Value = Current.String

  module Key = struct
    (* TODO: add more things in the key, like the global configuration *)
    type t = { deps : output list; prep : Prep.t; blessed : bool; voodoo : Current_git.Commit.t }

    let digest { deps; prep; blessed; voodoo } =
      Fmt.str "%s-%s-%s-%a-%s" (Bool.to_string blessed)
        (Prep.package prep |> Package.digest)
        (Prep.artifacts_digest prep)
        Fmt.(list (fun f { artifacts_digest; _ } -> Fmt.pf f "%s" artifacts_digest))
        deps (Current_git.Commit.hash voodoo)
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let build No_context job Key.{ deps; prep; blessed; voodoo } =
    let open Lwt.Syntax in
    let switch = Current.Switch.create ~label:"prep cluster build" () in
    let* () = Current.Job.start ~level:Mostly_harmless job in
    let package = Prep.package prep in
    let folder = folder ~blessed package in

    let* _ = Folder_digest.sync ~job () in
    let artifacts_digest = Folder_digest.get folder in
    Current.Job.log job "Current artifacts digest for folder %a: %s" Fpath.pp folder
      (artifacts_digest |> Option.value ~default:"<empty>");

    (* TODO: invalidation *)
    if Option.is_some artifacts_digest then (
      Current.Job.log job "Using existing artifacts";
      Lwt.return_ok (artifacts_digest |> Option.get) )
    else
      let base = Misc.get_base_image package in

      let spec = spec ~artifacts_digest:(artifacts_digest |> Option.value ~default:"<empty>") ~voodoo ~base ~deps ~blessed prep in
      let Cluster_api.Obuilder_job.Spec.{ spec = `Contents spec } = Spec.to_ocluster_spec spec in
      let action = Cluster_api.Submission.obuilder_build spec in

      let version = Misc.base_image_version package in
      let cache_hint = "docs-universe-compile-" ^ version in
      Current.Job.log job "Using cache hint %S" cache_hint;

      let build_pool =
        Current_ocluster.Connection.pool ~job ~pool:Config.pool ~action ~cache_hint
          ~secrets:Config.ssh_secrets_values Config.ocluster_connection
      in
      let* build_job = Current.Job.use_pool ~switch job build_pool in
      Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
      let* result = Current_ocluster.Connection.run_job ~job build_job in
      let* () = Current.Switch.turn_off switch in
      match result with
      | Error (`Msg _) as e -> Lwt.return e
      | Ok _ ->
          let+ _ = Folder_digest.sync ~job () in
          let artifacts_digest = Folder_digest.get folder |> Option.get in
          Current.Job.log job "New artifacts digest => %s" artifacts_digest;
          Ok artifacts_digest
end

module CompileCache = Current_cache.Make (Compile)

let v ~name ~voodoo ~blessed ~deps prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep = prep and> voodoo = voodoo and> blessed = blessed and> deps = deps in
     let package = Prep.package prep in
     let blessed = Package.Blessed.is_blessed blessed package in
     let digest = CompileCache.get No_context Compile.Key.{ prep; blessed; voodoo; deps } in
     let opam = package |> Package.opam in
     let version = opam |> OpamPackage.version_to_string in
     let compile_folder = folder ~blessed package in
     let odoc =
       Mld.
         {
           file = Fpath.(parent compile_folder / (version ^ ".mld"));
           target = None;
           name = version;
           kind = Mld;
         }
     in
     Current.Primitive.map_result
       (function
         | Ok artifacts_digest -> Ok { package; blessed; odoc = Mld odoc; artifacts_digest }
         | Error e -> Error e)
       digest

let v ~voodoo ~blessed ~deps prep =
  let open Current.Syntax in
  let* b_prep = prep in
  let name = b_prep |> Prep.package |> Package.opam |> OpamPackage.to_string in
  v ~name ~voodoo ~blessed ~deps prep

let folder { package; blessed; _ } = folder ~blessed package
