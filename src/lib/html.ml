type hashes = { html_raw_hash : string } [@@deriving yojson]

type t = { package : Package.t; blessing : Package.Blessing.t; hashes : hashes }

let hashes t = t.hashes

let blessing t = t.blessing

let package t = t.package

let spec ~ssh ~generation ~base ~voodoo ~blessed compiled =
  let open Obuilder_spec in
  let package = Compile.package compiled in
  let linked_folder = Storage.folder (Linked (generation, blessed)) package in
  let raw_folder = Storage.folder (HtmlRaw (generation, blessed)) package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let version = opam |> OpamPackage.version_to_string in
  let tools = Voodoo.Gen.spec ~base voodoo |> Spec.finish in
  base |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [ "/home/opam/odoc"; "/home/opam/voodoo-gen" ]
           ~dst:"/home/opam/";
         run
           "mv ~/odoc $(opam config var bin)/odoc && cp ~/voodoo-gen $(opam config var \
            bin)/voodoo-gen";
         (* obtain the linked folder *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "rsync -aR %s:%s/./%s ." (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh) (Fpath.to_string linked_folder);
                "find . -name '*.tar' -exec tar -xvf {} \\;";
                "find . -type d -empty -delete";
              ];
         (* Run voodoo-gen *)
         workdir (Fpath.to_string (Storage.Base.generation_folder `Linked generation));
         run "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-gen -o %s -n %s --pkg-version %s"
           (Fpath.to_string (Storage.Base.folder (HtmlRaw generation)))
           name version;
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "echo '%f'" (Random.float 1.);
                Fmt.str "mkdir -p %a" Fpath.pp raw_folder;
                (* Extract raw and html output *)
                Fmt.str "rsync -aR ./%s %s:%s/." (Fpath.to_string raw_folder) (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh);
                (* Print hashes *)
                Fmt.str "set '%s' raw; %s" (Fpath.to_string raw_folder)
                  (Storage.hash_command ~prefix:"RAW");
              ];
       ]

let or_default a = function None -> a | b -> b

module Gen = struct
  type t = Epoch.t

  let id = "voodoo-gen"

  module Value = struct
    type t = hashes [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  module Key = struct
    type t = { config : Config.t; compile : Compile.t; voodoo : Voodoo.Gen.t }

    let key { config; compile; voodoo } =
      Fmt.str "v6-%s-%s-%s-%s"
        (Compile.package compile |> Package.digest)
        (Compile.hashes compile).linked_hash (Voodoo.Gen.digest voodoo) (Config.odoc config)

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ compile; _ } = Fmt.pf f "Voodoo gen %a" Package.pp (Compile.package compile)

  let auto_cancel = true

  let build generation job (Key.{ compile; voodoo; config } as key) =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let blessed = Compile.blessing compile in
    Current.Job.log job "Cache digest: %s" (Key.key key);
    let spec =
      spec ~ssh:(Config.ssh config) ~generation ~voodoo ~base:Misc.default_base_image ~blessed
        compile
    in
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
    let extract_hashes v_html_raw line =
      (* some early stopping could be done here *)
      let html_raw = Storage.parse_hash ~prefix:"RAW" line |> or_default v_html_raw in
      html_raw
    in
    let** html_raw = Misc.fold_logs build_job extract_hashes None in
    try
      let html_raw = Option.get html_raw in
      Lwt.return_ok { html_raw_hash = html_raw.hash }
    with Invalid_argument _ -> Lwt.return_error (`Msg "Gen: failed to parse output")
end

module GenCache = Current_cache.Make (Gen)

let v ~generation ~config ~name ~voodoo compile =
  let open Current.Syntax in
  Current.component "html %s" name
  |> let> compile = compile and> voodoo = voodoo and> generation = generation in
     let blessing = Compile.blessing compile in
     let package = Compile.package compile in
     let output = GenCache.get generation Gen.Key.{ compile; voodoo; config } in
     Current.Primitive.map_result (Result.map (fun hashes -> { package; blessing; hashes })) output
