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

let spec ~cache_key ~artifacts_digest ~base ~voodoo ~deps ~blessed prep =
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
         import_deps deps;
         (* obtain the prep folder *)
         Misc.rsync_pull ~digest:(Prep.artifacts_digest prep) [ prep_folder ];
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
           [ "/home/opam/odoc"; "/home/opam/voodoo-do" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         (* Run voodoo-do *)
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do -p %s %s" name
           (if blessed then "-b" else "");
         run "mkdir -p html";
         (* Extract compile output *)
         run ~secrets:Config.ssh_secrets ~network
           "rsync -avzR /home/opam/docs/./compile/ %s:%s/ && echo '%s'" Config.ssh_host
           Config.storage_folder artifacts_digest;
         (* Extract html output *)
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR /home/opam/docs/./html/ %s:%s/"
           Config.ssh_host Config.storage_folder;
         (* Compute compile folder digest *)
         run "%s" (Remote_cache.cmd_compute_sha256 [ compile_folder ]);
         run "%s" (Remote_cache.cmd_write_key cache_key [ compile_folder ]);
         (* Extract the digest info *)
         run ~secrets:Config.ssh_secrets ~network:Misc.network "%s" Remote_cache.cmd_sync_folder;
       ]

module Pool = struct
  (** The CompilePool module takes care of waching 
 and updating the compilation state of all packages *)

  type compile = t

  type t = {
    mutable values : compile Current_term.Output.t Package.Map.t;
    mutable watchers : compile Current_term.Output.t Lwt_condition.t Package.Map.t;
    mutex : Lwt_mutex.t;
  }

  let v () =
    { values = Package.Map.empty; watchers = Package.Map.empty; mutex = Lwt_mutex.create () }

  let pp_output =
    Current_term.Output.pp (fun f (t : compile) ->
        Fmt.pf f "%a-%s" Package.pp (package t) (artifacts_digest t))
  
  let status_eq = Result.equal 
    ~ok:(fun a b -> digest a = digest b)
    ~error:(Stdlib.(=))

  let update t package status =
    Lwt.async @@ fun () ->
    Lwt_mutex.with_lock t.mutex @@ fun () ->
    Fmt.pr "%a => %a\n" Package.pp package pp_output status;
    ( match Package.Map.find_opt package t.watchers, Package.Map.find_opt package t.values with
    | Some condition, None -> Lwt_condition.broadcast condition status
    | Some condition, Some status' when not (status_eq status status') -> Lwt_condition.broadcast condition status
    | _ -> () );
    t.values <- Package.Map.add package status t.values;
    Lwt.return_unit

  let watch t ~f package =
    let open Lwt.Syntax in
    let condition =
      Lwt_mutex.with_lock t.mutex @@ fun () ->
      match Package.Map.find_opt package t.watchers with
      | None ->
          let condition = Lwt_condition.create () in
          t.watchers <- Package.Map.add package condition t.watchers;
          Lwt.return condition
      | Some condition -> Lwt.return condition
    in
    let watch () =
      let* condition = condition in
      let rec aux () =
        let* v = Lwt_condition.wait condition in
        f v;
        aux ()
      in
      aux ()
    in

    let cancel, set_cancel = Lwt.wait () in
    Lwt.async (fun () ->
        Lwt.pick
          [
            watch ();
            (let+ () = cancel in
             failwith "Job cancelled");
          ]);
    set_cancel
end

module Monitor = struct
  let state_output =
    let v = function `Active `Ready -> 1 | `Active `Running -> 2 | `Msg _ -> 3 in
    List.fold_left
      (fun a b ->
        match (a, b) with
        | Ok a, Ok b -> Ok (b :: a)
        | Ok _, Error b -> Error b
        | a, Ok _ -> a
        | Error a, Error b when v a < v b -> Error b
        | a, _ -> a)
      (Ok [])

  let make (pool : Pool.t) (prep : Prep.t) =
    let targets = Prep.package prep |> Package.universe |> Package.Universe.deps in
    let state =
      ref
        ( List.to_seq targets
        |> Seq.map (fun t -> (t, Error (`Active `Ready)))
        |> Package.Map.of_seq )
    in
    let read () =
      Package.Map.bindings !state |> List.map snd
      |> state_output 
      |> Lwt.return_ok
    in
    let watch refresh =
      let cancel =
        List.map
          (fun target ->
            Pool.watch pool
              ~f:(fun value ->
                state := Package.Map.add target value !state;
                refresh ())
              target)
          targets
      in
      Lwt.return @@ fun () ->
      List.iter (fun u -> Lwt.wakeup_later u ()) cancel;
      Lwt.return_unit
    in
    let pp f = Fmt.pf f "Watch compilation status for %a" Prep.pp prep in
    Current.Monitor.create ~read ~watch ~pp

  let v pool prep =
    let open Current.Syntax in
    let* component =
      Current.component "Monitor compilation status"
      |> let> prep = prep in
         make pool prep |> Current.Monitor.get
    in
    Current.of_output component
end

module Compile = struct
  type output = t

  type t = Remote_cache.t

  let id = "voodoo-do"

  module Value = Current.String

  module Key = struct
    (* TODO: add more things in the key, like the global configuration *)
    type t = {
      deps : output list;
      prep : Prep.t;
      blessed : bool;
      voodoo : Voodoo.Do.t;
      compile_cache : Remote_cache.cache_entry;
    }

    let key { deps; prep; blessed; voodoo; compile_cache } =
      Fmt.str "%s-%s-%s-%a-%s-%s-%s" (Bool.to_string blessed)
        (Prep.package prep |> Package.digest)
        (Prep.artifacts_digest prep)
        Fmt.(list (fun f { artifacts_digest; _ } -> Fmt.pf f "%s" artifacts_digest))
        deps (Voodoo.Do.digest voodoo)
        (Remote_cache.digest compile_cache)
        Config.odoc

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let remote_cache_key Key.{ voodoo; prep; deps; _ } =
    (* When this key changes, the remote artifacts will be invalidated. *)
    let deps_digest =
      Fmt.to_to_string
        Fmt.(list (fun f { artifacts_digest; _ } -> Fmt.pf f "%s" artifacts_digest))
        deps
      |> Digest.string |> Digest.to_hex
    in
    Fmt.str "voodoo-compile-v0-%s-%s-%s-%s" (Prep.artifacts_digest prep) deps_digest
      (Voodoo.Do.digest voodoo)
      (Config.odoc |> Digest.string |> Digest.to_hex)

  let build digests job (Key.{ deps; prep; blessed; voodoo; compile_cache } as key) =
    let open Lwt.Syntax in
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
        let spec = spec ~cache_key ~artifacts_digest:"" ~voodoo ~base ~deps ~blessed prep in
        let Cluster_api.Obuilder_job.Spec.{ spec = `Contents spec } = Spec.to_ocluster_spec spec in
        let action = Cluster_api.Submission.obuilder_build spec in
        let version = Misc.base_image_version package in
        let cache_hint = "docs-universe-compile-" ^ version in
        let build_pool =
          Current_ocluster.Connection.pool ~job ~pool:Config.pool ~action ~cache_hint
            ~secrets:Config.ssh_secrets_values Config.ocluster_connection
        in
        let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
        Current.Job.log job "Using cache hint %S" cache_hint;
        Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
        let* result = Current_ocluster.Connection.run_job ~job build_job in
        match result with
        | Error (`Msg _) as e -> Lwt.return e
        | Ok _ ->
            let+ () = Remote_cache.sync ~job () in
            let artifacts_digest =
              Remote_cache.get digests folder |> Remote_cache.folder_digest_exn
            in
            Current.Job.log job "New artifacts digest => %s" artifacts_digest;
            Ok artifacts_digest )
end

module CompileCache = Current_cache.Make (Compile)

let v ~name ~voodoo ~cache ~blessed ~deps prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep = prep
     and> voodoo = voodoo
     and> cache = cache
     and> blessed = blessed
     and> deps = deps in
     let package = Prep.package prep in
     let blessed = Package.Blessed.is_blessed blessed package in
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
       CompileCache.get cache Compile.Key.{ prep; blessed; voodoo; deps; compile_cache }
     in
     Current.Primitive.map_result
       (Result.map (fun artifacts_digest -> { package; blessed; odoc = Mld odoc; artifacts_digest }))
       digest

let v ~voodoo ~cache ~blessed ~deps prep =
  let open Current.Syntax in
  let* b_prep = prep in
  let name = b_prep |> Prep.package |> Package.opam |> OpamPackage.to_string in
  v ~name ~voodoo ~cache ~blessed ~deps prep

let folder { package; blessed; _ } = folder ~blessed package
