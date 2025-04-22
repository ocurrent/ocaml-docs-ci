type hashes = {
  compile_hash : string option;
  linked_hash : string option;
  html_hash : string option;
}
[@@deriving yojson]

type t = { package : Package.t; blessing : Package.Blessing.t; hashes : hashes }
type jobty = CompileAndLink | CompileOnly | LinkOnly [@@deriving yojson]

let pp_jobty =
  let s = function
    | CompileAndLink -> "CompileAndLink"
    | CompileOnly -> "CompileOnly"
    | LinkOnly -> "LinkOnly"
  in
  Fmt.of_to_string s

let hashes t = t.hashes
let blessing t = t.blessing
let package t = t.package

let spec_success ~ssh ~base ~odoc_driver_base ~odoc_pin ~sherlodoc_pin ~config
    ~deps ~blessing ~generation ~jobty prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let prep_folder = Storage.folder Prep package in
  let prep0_folder = Storage.folder Prep0 package in
  let compile_folder =
    Storage.folder (Compile (generation, blessing)) package
  in
  let linked_folder = Storage.folder (Linked (generation, blessing)) package in
  let raw_folder = Storage.folder (HtmlRaw (generation, blessing)) package in
  let opam = package |> Package.opam in
  let name = opam |> OpamPackage.name_to_string in
  let tools = Voodoo.Odoc.spec ~base config |> Spec.finish in
  (* let compile_caches =
    deps |> List.map (fun
      {blessing; package; _} ->
        let dir = Storage.folder (Compile blessing) package in
        let fpath_strs = Fpath.segs dir in
        let cache_name = String.concat "--" fpath_strs in
        let dir = Fpath.(v "/home/opam/.cache/" // dir) in
        Obuilder_spec.Cache.v cache_name ~target:(Fpath.to_string dir)
        ) in *)
  let common =
    List.map
      (fun { blessing; package; _ } ->
        let dir = Storage.folder (Compile (generation, blessing)) package in
        Fmt.str "~/docs/docs-ci-scripts/download_compiled.sh %s %s %s %s %b"
          (Config.Ssh.host ssh)
          (Config.Ssh.storage_folder ssh)
          (Fpath.to_string dir)
          (Package.digest package ^ "-" ^ Epoch.digest generation)
          (Package.should_cache package))
      deps
    @ [
        Fmt.str "~/docs/docs-ci-scripts/get_prep_for_compile.sh %s %s %s %s"
          (Config.Ssh.host ssh)
          (Config.Ssh.storage_folder ssh)
          (Fpath.to_string prep0_folder)
          (Fpath.to_string prep_folder);
        "export OCAMLRUNPAM=b";
      ]
  in
  let post_compile =
    [
      Misc.tar_cmd compile_folder;
      Fmt.str "time rsync -aR ./%s %s:%s/."
        (Fpath.to_string compile_folder)
        (Config.Ssh.host ssh)
        (Config.Ssh.storage_folder ssh);
      Fmt.str "set '%s'; %s"
        (Fpath.to_string compile_folder)
        (Storage.Tar.hash_command ~prefix:"COMPILE" ());
    ]
  in
  let post_link_and_html =
    [
      Fmt.str "mkdir -p linked && mkdir -p %a && mv linked %a/" Fpath.pp
        (Storage.Base.generation_folder generation)
        Fpath.pp
        (Storage.Base.generation_folder generation);
      Fmt.str "mkdir -p %a" Fpath.pp linked_folder;
      Misc.tar_cmd linked_folder;
      Fmt.str "echo raw_folder: %s" (Fpath.to_string raw_folder);
      Fmt.str "~/docs/docs-ci-scripts/gen_status_json.sh %s"
        (Fpath.to_string raw_folder);
      Fmt.str "time rsync -aR ./%s %s:%s/."
        Fpath.(to_string (parent linked_folder))
        (Config.Ssh.host ssh)
        (Config.Ssh.storage_folder ssh);
      Fmt.str "echo '%f'" (Random.float 1.);
      Fmt.str "set '%s'; %s"
        (Fpath.to_string linked_folder)
        (Storage.Tar.hash_command ~prefix:"LINKED" ());
      Fmt.str "echo '%f'" (Random.float 1.);
      Fmt.str "mkdir -p %a" Fpath.pp raw_folder;
      (* Extract raw and html output *)
      Fmt.str "time rsync -aR ./%s %s:%s/."
        (Fpath.to_string raw_folder)
        (Config.Ssh.host ssh)
        (Config.Ssh.storage_folder ssh);
      (* Print hashes *)
      Fmt.str "set '%s' raw; %s"
        (Fpath.to_string raw_folder)
        (Storage.hash_command ~prefix:"RAW");
      "rm -rf /home/opam/docs/*";
    ]
  in
  let compile_caches =
    [
      Obuilder_spec.Cache.v "compile-cache"
        ~target:
          (Fmt.str "/home/opam/.cache/%a/compile" Fpath.pp
             (Storage.Base.generation_folder generation));
    ]
  in
  let odoc_driver =
    Voodoo.OdocDriver.spec ~base:odoc_driver_base ~odoc_pin ~sherlodoc_pin
    |> Spec.finish
  in
  base
  |> Spec.children ~name:"tools" tools
  |> Spec.children ~name:"odoc_driver" odoc_driver
  |> Spec.add
       ([
          workdir "/home/opam/docs/";
          run "sudo chown opam:opam . ";
          (* Import odoc and voodoo-do *)
          copy ~from:(`Build "tools") [ "/home/opam/odoc" ] ~dst:"/home/opam/";
          copy ~from:(`Build "odoc_driver")
            [
              "/home/opam/odoc_driver_voodoo";
              "/home/opam/sherlodoc";
              "/home/opam/odoc-md";
            ]
            ~dst:"/home/opam/";
          run "mv ~/sherlodoc $(opam config var bin)/sherlodoc";
          run ~network:Misc.network "sudo apt install -y jq";
          run ~network:Misc.network
            "git clone https://github.com/jonludlam/docs-ci-scripts.git && \
             echo %s"
            Config.random;
          (* obtain the compiled dependencies, prep folder and extract it *)
        ]
       @ [
           run ~network:Misc.network ~cache:compile_caches
             ~secrets:Config.Ssh.secrets "%s"
           @@ Misc.Cmd.list
                (Fmt.str "ssh -MNf %s" (Config.Ssh.host ssh)
                :: (* "rm -rf /home/opam/.cache/compile/*" :: *)
                   (common
                   @ [
                       Fmt.str
                         "time opam exec -- /home/opam/odoc_driver_voodoo \
                          --verbose --odoc /home/opam/odoc --odoc-md \
                          /home/opam/odoc-md --odoc-dir %a/compile --odocl-dir \
                          linked --html-dir %s --stats %s %s %s"
                         Fpath.pp
                         (Storage.Base.generation_folder generation)
                         (Fpath.to_string
                            (Storage.Base.folder (HtmlRaw generation)))
                         name
                         (match jobty with
                         | CompileAndLink -> ""
                         | CompileOnly -> "--actions compile-only"
                         | LinkOnly -> "--actions link-and-gen")
                         (match blessing with
                         | Blessed -> "--blessed"
                         | Universe -> "");
                       (* Fmt.str "/home/opam/odoc support-files -o %s"
                  (Fpath.to_string (Storage.Base.folder (HtmlRaw generation))); *)
                       Fmt.str "jq . driver-benchmarks.json || true";
                     ]
                   @ (match jobty with LinkOnly -> [] | _ -> post_compile)
                   @
                   match jobty with
                   | CompileOnly -> []
                   | _ -> post_link_and_html));
         ])

let spec_failure ~ssh ~base ~config ~blessing ~generation prep =
  let open Obuilder_spec in
  let package = Prep.package prep in
  let prep_folder = Storage.folder Prep package in
  let compile_folder =
    Storage.folder (Compile (generation, blessing)) package
  in
  let linked_folder = Storage.folder (Linked (generation, blessing)) package in
  let opam = Package.opam package in
  let name = OpamPackage.name_to_string opam in
  let version = OpamPackage.version_to_string opam in
  let tools = Voodoo.Odoc.spec ~base config |> Spec.finish in
  base
  |> Spec.children ~name:"tools" tools
  |> Spec.add
       [
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         (* Import odoc and voodoo-do *)
         copy ~from:(`Build "tools")
           [
             "/home/opam/odoc";
             "/home/opam/odoc_driver_voodoo";
             "/home/opam/sherlodoc";
             "/home/opam/odoc-md";
           ]
           ~dst:"/home/opam/";
         run "mv ~/odoc $(opam config var bin)/odoc";
         run "mv ~/sherlodoc $(opam config var bin)/sherlodoc";
         (* obtain the prep folder (containing opam.err.log) and extract it *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "rsync -aR %s:%s/./%s ." (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh)
                  (Fpath.to_string prep_folder);
                Fmt.str "find . -name '*.tar' -exec tar -xf {} \\;";
              ];
         (* prepare the compilation folder *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "mkdir -p %a" Fpath.pp compile_folder;
                Fmt.str
                  "rm -f compile/packages.mld compile/page-packages.odoc \
                   compile/packages/*.mld compile/packages/*.odoc \
                   compile/packages/%s/*.odoc"
                  name;
              ];
         (* Run voodoo-do && tar compile/linked output *)
         run "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str
                  "OCAMLRUNPARAM=b opam exec -- /home/opam/voodoo-do --failed \
                   -p %s %s "
                  name
                  (match blessing with Blessed -> "-b" | Universe -> "");
                Misc.tar_cmd compile_folder;
                Fmt.str "mkdir -p linked && mkdir -p %a && mv linked %a/"
                  Fpath.pp
                  (Storage.Base.generation_folder generation)
                  Fpath.pp
                  (Storage.Base.generation_folder generation);
                Fmt.str "mkdir -p %a" Fpath.pp linked_folder;
                Fmt.str "touch %a/hack" Fpath.pp linked_folder;
                Misc.tar_cmd linked_folder;
              ];
         (* Extract compile output   - cache needs to be invalidated if we want to be able to read the logs *)
         run ~network:Misc.network ~secrets:Config.Ssh.secrets "%s"
         @@ Misc.Cmd.list
              [
                Fmt.str "echo '%f'" (Random.float 1.);
                Fmt.str "rsync -aR ./%s ./%s %s:%s/."
                  (Fpath.to_string compile_folder)
                  Fpath.(to_string (parent linked_folder))
                  (Config.Ssh.host ssh)
                  (Config.Ssh.storage_folder ssh);
                Fmt.str "set '%s'; %s"
                  (Fpath.to_string compile_folder)
                  (Storage.Tar.hash_command ~prefix:"COMPILE" ());
                Fmt.str "set '%s'; %s"
                  (Fpath.to_string linked_folder)
                  (Storage.Tar.hash_command
                     ~extra_files:[ "../page-" ^ version ^ ".odocl" ]
                     ~prefix:"LINKED" ());
              ];
       ]

let or_default a = function None -> a | b -> b

let extract_hashes ((v_compile, v_linked), retriable_errors) line =
  let retry_conditions log_line =
    let retry_on =
      [
        "Temporary failure";
        "Could not resolve host";
        "rsync: connection unexpectedly closed";
        "Disconnected: Switch turned off";
      ]
    in
    List.fold_left
      (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
      false retry_on
  in
  let escape_on_success log_line =
    let escape_on = [ "Job succeeded" ] in
    List.fold_left
      (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
      false escape_on
  in
  (* some early stopping could be done here *)
  let compile =
    Storage.parse_hash ~prefix:"COMPILE" line |> or_default v_compile
  in
  let linked =
    Storage.parse_hash ~prefix:"LINKED" line |> or_default v_linked
  in
  if escape_on_success line then ((compile, linked), [])
    (* ignore retriable errors if the job has succeeded *)
  else if retry_conditions line then
    ((compile, linked), line :: retriable_errors)
  else ((compile, linked), retriable_errors)

module Compile = struct
  type output = t
  type t = { generation : Epoch.t }

  let id = "voodoo-do"

  module Value = struct
    type t = hashes [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  module Key = struct
    type t = {
      config : Config.t;
      odoc : string;
      sherlodoc : string;
      deps : output list;
      prep : Prep.t;
      base : Spec.t;
      odoc_driver_base : Spec.t;
      jobty : jobty;
      blessing : Package.Blessing.t;
    }

    let key
        {
          config = _;
          odoc;
          sherlodoc;
          deps;
          prep;
          blessing;
          base = _;
          odoc_driver_base = _;
          jobty;
        } =
      let odoc_sherl_hash =
        Fmt.str "%s-%s" odoc sherlodoc |> Digest.string |> Digest.to_hex
      in
      Fmt.str "v10-%s-%s-%s-%s-%a-%a" odoc_sherl_hash
        (Package.Blessing.to_string blessing)
        (Prep.package prep |> Package.digest)
        (Prep.hash prep)
        Fmt.(
          list (fun f { hashes = { compile_hash; _ }; _ } ->
              Fmt.pf f "%a" (Fmt.option string) compile_hash))
        deps pp_jobty jobty

    let digest t = key t |> Digest.string |> Digest.to_hex
  end

  let pp f Key.{ prep; _ } =
    Fmt.pf f "Voodoo do %a" Package.pp (Prep.package prep)

  let auto_cancel = true

  let build { generation; _ } job
      Key.
        {
          deps;
          prep;
          blessing;
          config;
          base;
          odoc_driver_base;
          odoc;
          sherlodoc;
          jobty;
        } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let package = Prep.package prep in
    let odoc_pin = odoc in
    let sherlodoc_pin = sherlodoc in
    Current.Job.write job
      (Fmt.str "Prep base: %s" (Spec.to_spec (Prep.base prep)));
    let** spec =
      match Prep.result prep with
      | Success ->
          Lwt.return_ok
            (spec_success ~generation ~ssh:(Config.ssh config) ~config ~base
               ~odoc_driver_base ~odoc_pin ~sherlodoc_pin ~deps ~blessing ~jobty
               prep)
      | Failed ->
          Lwt.return_ok
            (spec_failure ~generation ~ssh:(Config.ssh config) ~config ~base
               ~blessing prep)
    in
    let action = Misc.to_ocluster_submission spec in
    let version = Misc.cache_hint package in
    let cache_hint = "docs-universe-compile-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action
        ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_do config)
    in
    let* build_job =
      Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.write job
      (Fmt.str
         "@.To reproduce locally:@.@.cat > prep.spec \
          <<'END-OF-SPEC'@.\o033[34m%s\o033[0m@.END-OF-SPEC@.@.ocluster-client \
          submit-obuilder --local-file prep.spec \\@.--pool linux-x86_64 \
          --connect ocluster-submission.cap --cache-hint %s \\@.--secret \
          ssh_privkey:id_rsa --secret ssh_pubkey:id_rsa.pub--secret \
          ssh_config:ssh_config@.@."
         (Spec.to_spec spec) cache_hint);

    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    let fn () =
      let** _ = Current_ocluster.Connection.run_job ~job build_job in
      Misc.fold_logs build_job extract_hashes ((None, None), [])
    in
    let** compile, linked =
      Retry.retry_loop ~job ~log_string:(Current.Job.id job) fn
    in
    let extract_hashes v_html_raw line =
      (* some early stopping could be done here *)
      let html_raw =
        Storage.parse_hash ~prefix:"RAW" line |> or_default v_html_raw
      in
      html_raw
    in
    let** html_raw = Misc.fold_logs build_job extract_hashes None in

    try
      let h (x : Storage.id_hash) = x.hash in
      let compile = Option.map h compile in
      let linked = Option.map h linked in
      let html_raw = Option.map h html_raw in
      Lwt.return_ok
        { compile_hash = compile; linked_hash = linked; html_hash = html_raw }
    with Invalid_argument _ ->
      Lwt.return_error (`Msg "Compile: failed to parse output")
end

module CompileCache = Current_cache.Make (Compile)

let v ~generation ~config ~name ~blessing ~deps ~jobty prep =
  let open Current.Syntax in
  Current.component "do %s" name
  |> let> prep
     and> blessing
     and> deps
     and> odoc_driver_base = Misc.default_base_image in
     let package = Prep.package prep in
     let base = Prep.base prep in
     Logs.debug (fun m -> m "Prep base: %s" (Spec.to_spec base));
     let odoc = Config.odoc config in
     let sherlodoc = Config.sherlodoc config in
     let output =
       CompileCache.get { Compile.generation }
         Compile.Key.
           {
             prep;
             blessing;
             deps;
             config;
             base;
             odoc_driver_base;
             odoc;
             sherlodoc;
             jobty;
           }
     in
     Current.Primitive.map_result
       (Result.map (fun hashes -> { package; blessing; hashes }))
       output
