type hashes = {
  html_tailwind_commit_hash : string;
  html_tailwind_tree_hash : string;
  html_classic_commit_hash : string;
  html_classic_tree_hash : string;
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

let tailwind_folder ~blessed package = Fpath.(v "tailwind" // base_folder ~blessed package)

let classic_folder ~blessed package = Fpath.(v "html" // base_folder ~blessed package)

let spec ~ssh ~cache_key ~base ~voodoo ~blessed compiled =
  let open Obuilder_spec in
  let package = Compile.package compiled in
  let commit = (Compile.hashes compiled).linked_commit_hash in
  let tailwind_folder = tailwind_folder ~blessed package in
  let classic_folder = classic_folder ~blessed package in
  let branch = Git_store.Branch.v package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let version = opam |> OpamPackage.version_to_string in
  let message = Fmt.str "docs ci update %s\n\n%s" (Fmt.to_to_string Package.pp package) cache_key in
  let tools = Voodoo.Gen.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* obtain the linked folder *)
         Git_store.Cluster.pull_to_directory ~repository:Linked ~ssh ~directory:"linked"
           ~branches:[ (branch, `Commit commit) ];
         run "find .";
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         run "cp ~/voodoo-gen $(opam config var bin)/voodoo-gen";
         (* Run voodoo-do *)
         run
           "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-gen pkgver -o tailwind -n %s \
            --pkg-version %s"
           name version;
         run
           "opam exec -- bash -c 'for i in $(find linked -name *.odocl); do odoc html-generate $i \
            -o html; done'";
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp tailwind_folder;
         run "%s" @@ Fmt.str "mkdir -p %a" Fpath.pp classic_folder;
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run "echo '%f'" (Random.float 1.);
         (* Extract html/tailwind output *)
         Git_store.Cluster.write_folder_to_git ~repository:HtmlTailwind ~ssh ~branch
           ~folder:"tailwind" ~message ~git_path:"/tmp/git-html-tailwind";
         (* Extract html output*)
         Git_store.Cluster.write_folder_to_git ~repository:HtmlClassic ~ssh ~branch ~folder:"html"
           ~message ~git_path:"/tmp/git-html-classic";
         run "cd /tmp/git-html-tailwind && %s"
           (Git_store.print_branches_info ~prefix:"TAILWIND" ~branches:[ branch ]);
         run "cd /tmp/git-html-classic && %s"
           (Git_store.print_branches_info ~prefix:"HTML" ~branches:[ branch ]);
       ]

let or_default a = function None -> a | b -> b

module Gen = struct
  type t = No_context

  let id = "voodoo-gen"

  module Value = struct
    type t = hashes [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  module Key = struct
    type t = { config : Config.t; compile : Compile.t; voodoo : Voodoo.Gen.t }

    let key { config; compile; voodoo } =
      Fmt.str "v3-%s-%s-%s-%s"
        (Compile.package compile |> Package.digest)
        (Compile.hashes compile).linked_tree_hash (Voodoo.Gen.digest voodoo) (Config.odoc config)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ compile; _ } = Fmt.pf f "Voodoo gen %a" Package.pp (Compile.package compile)

  let auto_cancel = true

  let remote_cache_key Key.{ voodoo; compile; config; _ } =
    Fmt.str "voodoo-gen-v0-%s-%s-%s-%s"
      (Compile.package compile |> Package.digest)
      (Compile.hashes compile).linked_tree_hash (Voodoo.Gen.digest voodoo)
      (Config.odoc config |> Digest.string |> Digest.to_hex)

  let build No_context job (Key.{ compile; voodoo; config } as key) =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let blessed = Compile.is_blessed compile in
    let cache_key = remote_cache_key key in
    Current.Job.log job "Cache digest: %s" (Key.key key);
    let spec = spec ~ssh:(Config.ssh config) ~cache_key ~voodoo ~base:Misc.default_base_image ~blessed compile in
    let action = Misc.to_ocluster_submission spec in
    let cache_hint = "docs-universe-gen" in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_gen config)
    in
    let* build_job = Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let** _ = Current_ocluster.Connection.run_job ~job build_job in
    let extract_hashes (v_html_tailwind, v_html_classic) line =
      (* some early stopping could be done here *)
      let html_tailwind =
        Git_store.parse_branch_info ~prefix:"TAILWIND" line |> or_default v_html_tailwind
      in
      let html_classic =
        Git_store.parse_branch_info ~prefix:"HTML" line |> or_default v_html_classic
      in
      (html_tailwind, html_classic)
    in
    let** html_tailwind, html_classic = Misc.fold_logs build_job extract_hashes (None, None) in
    try
      let html_tailwind = Option.get html_tailwind in
      let html_classic = Option.get html_classic in

      Lwt.return_ok
        {
          html_tailwind_commit_hash = html_tailwind.commit_hash;
          html_tailwind_tree_hash = html_tailwind.tree_hash;
          html_classic_commit_hash = html_classic.commit_hash;
          html_classic_tree_hash = html_classic.tree_hash;
        }
    with Invalid_argument _ -> Lwt.return_error (`Msg "Gen: failed to parse output")
end

module GenCache = Current_cache.Make (Gen)

let v ~config ~name ~voodoo compile =
  let open Current.Syntax in
  Current.component "html %s" name
  |> let> compile = compile and> voodoo = voodoo in
     let blessed = Compile.is_blessed compile in
     let package = Compile.package compile in
     let output = GenCache.get No_context Gen.Key.{ compile; voodoo; config } in
     Current.Primitive.map_result (Result.map (fun hashes -> { package; blessed; hashes })) output
