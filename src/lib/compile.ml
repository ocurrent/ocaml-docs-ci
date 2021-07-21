type hashes = { compile_hash : string; linked_hash : string } [@@deriving yojson]

type t = { package : Package.t; blessing : Package.Blessing.t; hashes : hashes }

let hashes t = t.hashes

let blessing t = t.blessing

let package t = t.package

let spec_success ~ssh ~base ~voodoo ~deps ~blessing ~generation prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let prep_folder = Storage.folder Prep package in
  let compile_folder = Storage.folder (Compile blessing) package in
  let linked_folder = Storage.folder (Linked (generation, blessing)) package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         (* obtain the compiled dependencies, prep folder and extract it *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Storage.for_all
                  ( deps
                  |> List.rev_map (fun { blessing; package; _ } ->
                         (Storage.Compile blessing, package)) )
                  (Fmt.str "rsync -aR %s:%s/./$1 .;" (Config.Ssh.host ssh)
                     (Config.Ssh.storage_folder ssh));
                Fmt.str "rsync -aR %s:%s/./%s ." (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh) (Fpath.to_string prep_folder);
                Fmt.str "find . -name '*.tar' -exec tar -xvf {} \\;";
              ];
         (* prepare the compilation folder *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
                Fmt.str
                  "rm -f compile/packages.mld compile/page-packages.odoc compile/packages/*.mld \
                   compile/packages/*.odoc compile/packages/%s/*.odoc"
                  name;
              ];
         (* Run voodoo-do && tar compile/linked output *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do -p %s %s " name
                  (match blessing with Blessed -> "-b" | Universe -> "");
                Misc.tar_cmd compile_folder;
                Fmt.str "mkdir -p linked && mkdir -p %a && mv linked %a/" Fpath.pp
                  (Storage.Base.generation_folder `Linked generation)
                  Fpath.pp
                  (Storage.Base.generation_folder `Linked generation);
                Fmt.str "mkdir -p %a" Fpath.pp linked_folder;
                Misc.tar_cmd linked_folder;
              ];
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "echo '%f'" (Random.float 1.);
                Fmt.str "rsync -aR ./%s ./%s %s:%s/." (Fpath.to_string compile_folder)
                  Fpath.(to_string (parent linked_folder))
                  (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
                Fmt.str "set '%s'; %s" (Fpath.to_string compile_folder)
                  (Storage.Tar.hash_command ~prefix:"COMPILE" ());
                Fmt.str "set '%s'; %s" (Fpath.to_string linked_folder)
                  (Storage.Tar.hash_command ~prefix:"LINKED" ());
              ];
       ]

let spec_failure ~ssh ~base ~voodoo ~blessing ~generation prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let prep_folder = Storage.folder Prep package in
  let compile_folder = Storage.folder (Compile blessing) package in
  let linked_folder = Storage.folder (Linked (generation, blessing)) package in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-do" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         (* obtain the prep folder (containing opam.err.log) and extract it *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "rsync -aR %s:%s/./%s ." (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh) (Fpath.to_string prep_folder);
                Fmt.str "find . -name '*.tar' -exec tar -xvf {} \\;";
              ];
         (* prepare the compilation folder *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
                Fmt.str
                  "rm -f compile/packages.mld compile/page-packages.odoc compile/packages/*.mld \
                   compile/packages/*.odoc compile/packages/%s/*.odoc"
                  name;
              ];
         (* Run voodoo-do && tar compile/linked output *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do --failed -p %s %s " name
                  (match blessing with Blessed -> "-b" | Universe -> "");
                Misc.tar_cmd compile_folder;
                Fmt.str "mkdir -p linked && mkdir -p %a && mv linked %a/" Fpath.pp
                  (Storage.Base.generation_folder `Linked generation)
                  Fpath.pp
                  (Storage.Base.generation_folder `Linked generation);
                Fmt.str "mkdir -p %a" Fpath.pp linked_folder;
                Misc.tar_cmd linked_folder;
              ];
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "echo '%f'" (Random.float 1.);
                Fmt.str "rsync -aR ./%s ./%s %s:%s/." (Fpath.to_string compile_folder)
                  Fpath.(to_string (parent linked_folder))
                  (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh);
                Fmt.str "set '%s'; %s" (Fpath.to_string compile_folder)
                  (Storage.Tar.hash_command ~prefix:"COMPILE" ());
                Fmt.str "set '%s'; %s" (Fpath.to_string linked_folder)
                  (Storage.Tar.hash_command
                     ~extra_files:[ "../page-" ^ version ^ ".odocl" ]
                     ~prefix:"LINKED" ());
              ];
       ]

let or_default a = function None -> a | b -> b

module Compile = struct
  type output = t

  type t = { generation : Epoch.t; }

  let id = "voodoo-do"

  module Value = struct
    type t = hashes [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  module Key = struct
    type t = {
      config : Config.t;
      deps : output list;
      prep : Prep.t;
      blessing : Package.Blessing.t;
      voodoo : Voodoo.Do.t;
    }

    let key { config; deps; prep; blessing; voodoo } =
      Fmt.str "v9-%s-%s-%s-%a-%s-%s"
        (Package.Blessing.to_string blessing)
        (Prep.package prep |> Package.digest)
        (Prep.hash prep)
        Fmt.(list (fun f { hashes = { compile_hash; _ }; _ } -> Fmt.pf f "%s" compile_hash))
        deps (Voodoo.Do.digest voodoo) (Config.odoc config)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let build { generation; _ } job Key.{ deps; prep; blessing; voodoo; config } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let package = Prep.package prep in
    let base = Misc.get_base_image package in
    let** spec =
      match Prep.result prep with
      | Success ->
          Lwt.return_ok
            (spec_success ~generation ~ssh:(Config.ssh config) ~voodoo ~base ~deps ~blessing prep)
      | Failed ->
          Lwt.return_ok
            (spec_failure ~generation ~ssh:(Config.ssh config) ~voodoo ~base ~blessing prep)
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
    let** _ = Current_ocluster.Connection.run_job ~job build_job in
    let extract_hashes (v_compile, v_linked) line =
      (* some early stopping could be done here *)
      let compile = Storage.parse_hash ~prefix:"COMPILE" line |> or_default v_compile in
      let linked = Storage.parse_hash ~prefix:"LINKED" line |> or_default v_linked in
      (compile, linked)
    in
    let** compile, linked = Misc.fold_logs build_job extract_hashes (None, None) in
    try
      let compile = Option.get compile in
      let linked = Option.get linked in
      Lwt.return_ok { compile_hash = compile.hash; linked_hash = linked.hash }
    with Invalid_argument _ -> Lwt.return_error (`Msg "Compile: failed to parse output")
end

module CompileCache = Current_cache.Make (Compile)

let v ~generation ~config ~name ~voodoo ~blessing ~deps prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep = prep
     and> voodoo = voodoo
     and> blessing = blessing
     and> deps = deps
     and> generation = generation in
     let package = Prep.package prep in
     let output =
       CompileCache.get
         { Compile.generation }
         Compile.Key.{ prep; blessing; voodoo; deps; config }
     in
     Current.Primitive.map_result (Result.map (fun hashes -> { package; blessing; hashes })) output
