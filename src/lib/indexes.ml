type t = unit

let network = Voodoo.network

let spec ~base (packages : Compile.t list) =
  let mld =
    Mld.Gen.v
      (List.map
         (fun comp -> (Compile.package comp, Compile.is_blessed comp, Compile.odoc comp))
         packages)
  in
  let open Obuilder_spec in
  base
  |> Spec.add
       [
         run ~network "opam pin -ny odoc %s && opam depext -iy odoc" Config.odoc;
         workdir "/home/opam/docs/";
         run "sudo chown opam:opam . ";
         Misc.rsync_pull [ Fpath.v "compile" ];
         run "find . -type d";
         run "%s" @@ Fmt.to_to_string Mld.Gen.pp_gen_files_commands mld;
         run "%s"
         @@ Fmt.str
              {|
           eval $(opam config env)
           %a
           %a
           find -maxdepth 4 -type f -name '*.odocl' -exec odoc html -o /home/opam/html {} \; # build index pages html
           odoc support-files -o /home/opam/html # build support files
           |}
           Mld.Gen.pp_compile_commands mld
           Mld.Gen.pp_link_commands mld;
         run ~secrets:Config.ssh_secrets ~network "rsync -avzR --exclude=\"/*/*/*/*/\" . %s:%s/test"
           Config.ssh_host Config.storage_folder;
         run ~secrets:Config.ssh_secrets ~network "rsync -avz /home/opam/html/ %s:%s/html"
           Config.ssh_host Config.storage_folder;
       ]

(*          run "find . -type f -name '*.cmt' -exec sh -c 'mv \"$1\" \"${1%%.cmt}.odoc\"' _ {} \\;"; *)
let v packages =
  let open Current.Syntax in
  let spec =
    let+ packages = packages in
    spec ~base:(Spec.make "ocaml/opam:ubuntu-ocaml-4.12") packages |> Spec.to_ocluster_spec
  in
  let conn = Current_ocluster.Connection.create ~max_pipeline:10 Config.cap in
  let cluster = Current_ocluster.v ~secrets:Config.ssh_secrets_values conn in
  let+ () =
    Current_ocluster.build_obuilder ~label:"build indexes" ~src:(Current.return [])
      ~pool:Config.pool ~cache_hint:"docs-universe-link" cluster spec
  in
  ()
