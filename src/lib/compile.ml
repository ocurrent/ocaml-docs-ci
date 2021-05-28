type t = { package : Package.t; blessed : bool; odoc : Mld.Gen.odoc_dyn; artifacts_digest : string }

let digest t =
  Package.digest t.package ^ Bool.to_string t.blessed ^ Mld.Gen.digest t.odoc ^ t.artifacts_digest

let artifacts_digest t = t.artifacts_digest

let is_blessed t = t.blessed

let odoc t = t.odoc

let package t = t.package

let network = Misc.network

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

let spec ~ssh ~branch ~remote_cache ~cache_key ~artifacts_digest ~base ~voodoo ~deps ~blessed prep =
  let open Obuilder_spec in
  let prep_folder = Prep.folder prep in
  let package = Prep.package prep in
  let compile_folder = folder ~blessed package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* obtain the compiled dependencies *)
         Spec.add_rsync_retry_script;
         import_deps ~ssh deps;
         (* obtain the prep folder *)
         Misc.rsync_pull ~ssh ~digest:(Prep.artifacts_digest prep) [ prep_folder ];
         run "find . -type d";
         (* prepare the compilation folder *)
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
         (* remove eventual leftovers (should not be needed)*)
         run
           "rm -f compile/packages.mld compile/page-packages.odoc compile/packages/*.mld \
            compile/packages/*.odoc";
         run "rm -f compile/packages/%s/*.odoc" name;
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do"; "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         run "cp ~/voodoo-gen $(opam config var bin)/voodoo-gen";
         (* Run voodoo-do *)
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do -p %s %s" name
           (if blessed then "-b" else "");
         run "mkdir -p html";
         (* Extract compile output *)
         run ~secrets:Config.Ssh.secrets ~network
           "rsync -avzR /home/opam/docs/./compile/ %s:%s/ && echo '%s'" (Config.Ssh.host ssh)
           (Config.Ssh.storage_folder ssh) (artifacts_digest ^ cache_key);
         (* Extract html/tailwind output *)
         Git_store.Cluster.clone ~branch ~directory:"git-store" ssh;
         run "rm -rf git-store/html && mv html/tailwind git-store/html";
         workdir "git-store";
         run "git add --all";
         run "git commit -m 'docs ci update %s\n\n%s' --allow-empty"
           (Fmt.to_to_string Package.pp package)
           cache_key;
         Git_store.Cluster.push ssh;
         workdir "..";
         (* extract html output*)
         run ~secrets:Config.Ssh.secrets ~network "rsync -avzR /home/opam/docs/./html/ %s:%s/"
           (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
         (* Compute compile folder digest *)
         run "%s" (Remote_cache.cmd_compute_sha256 [ compile_folder ]);
         run "%s" (Remote_cache.cmd_write_key cache_key [ compile_folder ]);
         (* Extract the digest info *)
         run ~secrets:Config.Ssh.secrets ~network:Misc.network "%s"
           (Remote_cache.cmd_sync_folder remote_cache);
       ]

let git_update_pool = Current.Pool.create ~label:"git merge into live" 1

module Compile = struct
  type output = t

  type t = Remote_cache.t

  let id = "voodoo-do"

  module Value = Current.String

  module Key = struct
    type t = {
      config : Config.t;
      deps : output list;
      prep : Prep.t;
      blessed : bool;
      voodoo : Voodoo.Do.t;
      compile_cache : Remote_cache.cache_entry;
    }

    let key { config; deps; prep; blessed; voodoo; compile_cache } =
      Fmt.str "v1-%s-%s-%s-%a-%s-%s-%s" (Bool.to_string blessed)
        (Prep.package prep |> Package.digest)
        (Prep.artifacts_digest prep)
        Fmt.(list (fun f { artifacts_digest; _ } -> Fmt.pf f "%s" artifacts_digest))
        deps (Voodoo.Do.digest voodoo)
        (Remote_cache.digest compile_cache)
        (Config.odoc config)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let remote_cache_key Key.{ voodoo; prep; deps; config; _ } =
    (* When this key changes, the remote artifacts will be invalidated. *)
    let deps_digest =
      Fmt.to_to_string
        Fmt.(list (fun f { artifacts_digest; _ } -> Fmt.pf f "%s" artifacts_digest))
        deps
      |> Digest.string |> Digest.to_hex
    in
    Fmt.str "voodoo-compile-v1-%s-%s-%s-%s" (Prep.artifacts_digest prep) deps_digest
      (Voodoo.Do.digest voodoo)
      (Config.odoc config |> Digest.string |> Digest.to_hex)

  let build digests job (Key.{ deps; prep; blessed; voodoo; compile_cache; config } as key) =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let package = Prep.package prep in
    let folder = folder ~blessed package in
    let cache_key = remote_cache_key key in
    Current.Job.log job "Cache digest: %s" (Key.key key);
    match compile_cache with
    (* Here, we first look if there are already artifacts in the compilation folder.
       TODO: invalidation when the cache key changes. *)
    | Some (key, Ok compile_digest) when String.trim key = cache_key ->
        let* () = Current.Job.start ~level:Harmless job in
        Current.Job.log job "Using existing artifacts for %a: %s" Fpath.pp folder compile_digest;
        Lwt.return_ok compile_digest
    | Some (key, Failed) when String.trim key = cache_key ->
        let* () = Current.Job.start ~level:Harmless job in
        Current.Job.log job "Compile step failed for %a." Fpath.pp folder;
        Lwt.return_error (`Msg "Odoc compilation failed.")
    | _ -> (
        let base = Misc.get_base_image package in
        let branch = "html-" ^ (Prep.package prep |> Package.digest) in
        let spec =
          spec ~ssh:(Config.ssh config) ~branch ~remote_cache:digests ~cache_key
            ~artifacts_digest:"" ~voodoo ~base ~deps ~blessed prep
        in
        let action = Misc.to_ocluster_submission spec in
        let version = Misc.base_image_version package in
        let cache_hint = "docs-universe-compile-" ^ version in
        let build_pool =
          Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
            ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
            (Config.ocluster_connection_do config)
        in
        let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
        Current.Job.log job "Using cache hint %S" cache_hint;
        Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
        let* result = Current_ocluster.Connection.run_job ~job build_job in
        match result with
        | Error (`Msg _) as e -> Lwt.return e
        | Ok _ ->
            let ssh = Config.ssh config in
            let switch = Current.Switch.create ~label:"git merge pool switch" () in
            let* () = Current.Job.use_pool ~switch job git_update_pool in
            Lwt.catch
              (fun () ->
                let** () =
                  Git_store.Local.merge_to_live ~job ~ssh ~branch
                    ~msg:(Fmt.to_to_string Package.pp package)
                in
                let* () = Current.Switch.turn_off switch in
                let+ () = Remote_cache.sync ~job digests in
                let artifacts_digest =
                  Remote_cache.get digests folder |> Remote_cache.folder_digest_exn
                in
                Current.Job.log job "New artifacts digest => %s" artifacts_digest;
                Ok artifacts_digest)
              (fun exn ->
                let* () = Current.Switch.turn_off switch in
                raise exn))
end

module CompileCache = Current_cache.Make (Compile)

let v ~config ~name ~voodoo ~cache ~blessed ~deps prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep = prep
     and> voodoo = voodoo
     and> cache = cache
     and> blessed = blessed
     and> deps = deps in
     let package = Prep.package prep in
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
     let compile_cache = Remote_cache.get cache compile_folder in
     let digest =
       CompileCache.get cache Compile.Key.{ prep; blessed; voodoo; deps; compile_cache; config }
     in
     Current.Primitive.map_result
       (Result.map (fun artifacts_digest -> { package; blessed; odoc = Mld odoc; artifacts_digest }))
       digest

let v ~config ~voodoo ~cache ~blessed ~deps prep =
  let open Current.Syntax in
  let* b_prep = prep in
  let name = b_prep |> Prep.package |> Package.opam |> OpamPackage.to_string in
  v ~config ~name ~voodoo ~cache ~blessed ~deps prep

let folder { package; blessed; _ } = folder ~blessed package
