type hashes = {
  compile_commit_hash : string;
  compile_tree_hash : string;
  linked_commit_hash : string;
  linked_tree_hash : string;
}
[@@deriving yojson]

type t = { package : Package.t; blessed : bool; hashes : hashes }

let hashes t = t.hashes

let is_blessed t = t.blessed

let package t = t.package

let base_folder ~blessed package =
  let universe = Package.universe package |> Package.Universe.hash in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  if blessed then Fpath.(v "packages" / name / version)
  else Fpath.(v "universes" / universe / name / version)

let compile_folder ~blessed package = Fpath.(v "compile" // base_folder ~blessed package)

let linked_folder ~blessed package = Fpath.(v "linked" // base_folder ~blessed package)

let import_compile_deps ~ssh t =
  let branches =
    List.map
      (fun { package; hashes = { compile_commit_hash; _ }; _ } ->
        (Git_store.Branch.v package, `Commit compile_commit_hash))
      t
  in
  Git_store.Cluster.pull_to_directory ~repository:Compile ~ssh ~directory:"compile" ~branches

let spec ~ssh ~cache_key ~base ~voodoo ~deps ~blessed prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let compile_folder = compile_folder ~blessed package in
  let linked_folder = linked_folder ~blessed package in
  let branch = Git_store.Branch.v package in
  let branches = [ (branch, Fpath.to_string (base_folder ~blessed package)) ] in
  let commit = Prep.commit_hash prep in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let message = Fmt.str "docs ci update %s\n\n%s" (Fmt.to_to_string Package.pp package) cache_key in
  let tools = Voodoo.Do.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* obtain the compiled dependencies *)
         import_compile_deps ~ssh deps;
         (* obtain the prep folder *)
         Git_store.Cluster.pull_to_directory ~repository:Prep ~ssh ~directory:"prep"
           ~branches:[ (branch, `Commit commit) ];
         run "find . -name '*.tar' -exec tar -xvf {} \\;";
         (* prepare the compilation folder *)
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
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
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp linked_folder;
         (* tar compile/linked output *)
         run "%s && %s" (Misc.tar_cmd compile_folder) (Misc.tar_cmd linked_folder);
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run "echo '%f'" (Random.float 1.);
         Git_store.Cluster.write_folders_to_git ~repository:Compile ~ssh ~branches ~folder:"compile"
           ~message ~git_path:"/tmp/git-compile";
         Git_store.Cluster.write_folder_to_git ~repository:Linked ~ssh ~branch ~folder:"linked"
           ~message ~git_path:"/tmp/git-linked";
         run "cd /tmp/git-compile && %s"
           (Git_store.print_branches_info ~prefix:"COMPILE" ~branches:[ branch ]);
         run "cd /tmp/git-linked && %s"
           (Git_store.print_branches_info ~prefix:"LINKED" ~branches:[ branch ]);
       ]

let or_default a = function None -> a | b -> b

module Compile = struct
  type output = t

  type t = No_context

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
      blessed : bool;
      voodoo : Voodoo.Do.t;
    }

    let key { config; deps; prep; blessed; voodoo } =
      Fmt.str "v5-%s-%s-%s-%a-%s-%s" (Bool.to_string blessed)
        (Prep.package prep |> Package.digest)
        (Prep.tree_hash prep)
        Fmt.(
          list (fun f { hashes = { compile_tree_hash; _ }; _ } -> Fmt.pf f "%s" compile_tree_hash))
        deps (Voodoo.Do.digest voodoo) (Config.odoc config)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let remote_cache_key Key.{ voodoo; prep; deps; config; _ } =
    (* When this key changes, the remote artifacts will be invalidated. *)
    let deps_digest =
      Fmt.to_to_string
        Fmt.(
          list (fun f { hashes = { compile_tree_hash; _ }; _ } -> Fmt.pf f "%s" compile_tree_hash))
        deps
      |> Digest.string |> Digest.to_hex
    in
    Fmt.str "voodoo-compile-v2-%s-%s-%s-%s" (Prep.tree_hash prep) deps_digest
      (Voodoo.Do.digest voodoo)
      (Config.odoc config |> Digest.string |> Digest.to_hex)

  let build No_context job (Key.{ deps; prep; blessed; voodoo; config } as key) =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let package = Prep.package prep in
    let cache_key = remote_cache_key key in
    Current.Job.log job "Cache digest: %s" (Key.key key);
    let base = Misc.get_base_image package in
    let spec = spec ~ssh:(Config.ssh config) ~cache_key ~voodoo ~base ~deps ~blessed prep in
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
      let compile = Git_store.parse_branch_info ~prefix:"COMPILE" line |> or_default v_compile in
      let linked = Git_store.parse_branch_info ~prefix:"LINKED" line |> or_default v_linked in
      (compile, linked)
    in
    let** compile, linked = Misc.fold_logs build_job extract_hashes (None, None) in
    try
      let compile = Option.get compile in
      let linked = Option.get linked in

      Lwt.return_ok
        {
          compile_commit_hash = compile.commit_hash;
          compile_tree_hash = compile.tree_hash;
          linked_commit_hash = linked.commit_hash;
          linked_tree_hash = linked.tree_hash;
        }
    with Invalid_argument _ -> Lwt.return_error (`Msg "Compile: failed to parse output")
end

module CompileCache = Current_cache.Make (Compile)

let v ~config ~name ~voodoo ~blessed ~deps prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep = prep and> voodoo = voodoo and> blessed = blessed and> deps = deps in
     let package = Prep.package prep in
     let output = CompileCache.get No_context Compile.Key.{ prep; blessed; voodoo; deps; config } in
     Current.Primitive.map_result (Result.map (fun hashes -> { package; blessed; hashes })) output

let folder { package; blessed; _ } = compile_folder ~blessed package
