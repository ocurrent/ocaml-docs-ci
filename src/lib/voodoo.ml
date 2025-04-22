let network = [ "host" ]

let download_cache =
  Obuilder_spec.Cache.v "opam-archives"
    ~target:"/home/opam/.opam/download-cache"

let cache = [ download_cache ]

module Prep = struct
  let spec ~base =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run ~network ~cache "%s"
           @@ Misc.Cmd.list
                [
                  "sudo apt-get update && sudo apt-get install -yy m4 \
                   pkg-config";
                  Fmt.str "opam pin -ny opamh %s"
                    "https://github.com/jonludlam/opamh.git#d54dbe7991af6020c36f89c300d8109664541963 \
                     && opam install -y opamh";
                  "cp $(opam config var bin)/opamh /home/opam";
                  "rm -rf $(opam config var prefix)";
                ];
         ]
end

module Odoc = struct
  let spec ~base config =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run ~network ~cache "%s"
           @@ Misc.Cmd.list
                [
                  "sudo apt-get update && sudo apt-get install -yy m4";
                  Fmt.str
                    "opam pin -ny odoc-parser.dev %s && opam depext -iy \
                     odoc-parser"
                    (Config.odoc config);
                  Fmt.str "opam pin -ny odoc.dev %s && opam depext -iy odoc"
                    (Config.odoc config);
                  "cp $(opam config var bin)/odoc /home/opam";
                  "rm -rf $(opam config var prefix)";
                ];
         ]
end

module OdocDriver = struct
  let spec ~base ~odoc_pin ~sherlodoc_pin =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run ~network ~cache "%s"
           @@ Misc.Cmd.list
                [
                  "sudo apt-get update";
                  Fmt.str "opam pin -ny sherlodoc.dev %s" sherlodoc_pin;
                  Fmt.str
                    "opam pin -ny odoc-parser.dev %s && opam pin -ny \
                     odoc-md.dev %s &&  opam pin -ny odoc.dev %s && opam pin \
                     -ny odoc-driver.dev %s"
                    odoc_pin odoc_pin odoc_pin odoc_pin;
                  "opam install -y odoc-md";
                  "opam install -y odoc-driver sherlodoc";
                  "cp $(opam config var bin)/odoc_driver_voodoo $(opam config \
                   var bin)/sherlodoc $(opam config var bin)/odoc-md \
                   /home/opam";
                  "rm -rf $(opam config var prefix)";
                ];
         ]
end
