module Git = Current_git
module Docker = Current_docker.Default
module Fs = Current_fs

open Current.Syntax

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let _targets = ["unix"; "hvt"]
let _stages = ["tutorial"; "device-usage"; "applications" ]

let ocaml_4_11_1 ~base =
  let open Dockerfile in 
  from (Docker.Image.hash base) @@
  run "opam repo remove beta" @@
  run "opam repo set-url default git+https://github.com/ocaml/opam-repository.git" @@
  run "opam switch create 4.11.1"
  
let mirage_dev_dockerfile ~base =
  let open Dockerfile in 
  from (Docker.Image.hash base) @@
  copy ~src:["."] ~dst:"/src/mirage-dev" () @@
  run "ls /src/mirage-dev/packages/" @@
  run "opam repo add dune-universe git+https://github.com/dune-universe/opam-overlays.git" @@
  run "opam repo add mirage-dev /src/mirage-dev" @@
  run "opam pin add -n git+https://github.com/ocamllabs/duniverse.git" @@
  run "opam depext ocaml-freestanding solo5-bindings-hvt mirage opam-monorepo" @@
  run "opam install ocaml-freestanding" @@
  run "opam install solo5-bindings-hvt mirage opam-monorepo"  

let mirage_skeleton_dockerfile ~base=
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  env [("OCAMLFIND_CONF", "/home/opam/.opam/4.11.1/lib/findlib.conf")] @@ (* TODO: Dunified findlib is not working correctly.*)
  copy ~src:["."] ~dst:"/src/mirage-skeleton" ()

let test_unikernel_dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  volume "/home/opam/.cache/duniverse/" @@
  copy ~src:["./scripts/unikernel_test.sh"] ~dst:"/" () @@ 
  entrypoint_exec ["/unikernel_test.sh"]


let apply_dockerfile fn image =
  let+ base = image in
  `Contents (fn ~base)

let v ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci () =
  let src_mirage_dev = Git.Local.head_commit repo_mirage_dev in
  let src_mirage_skeleton = Git.Local.head_commit repo_mirage_skeleton in
  let src_mirage_ci = Git.Local.head_commit repo_mirage_ci 
  in
  let base = Docker.pull ~schedule:weekly "ocaml/opam2:4.11" in 
  let to_build = apply_dockerfile ocaml_4_11_1 base in
  let with_ocaml_4_11_1 = Docker.build ~label:"ocaml-4.11.1" ~pull:false ~dockerfile:to_build `No_context  (* Step 1: set ocaml 4.11.1*)
  in
  let to_build = apply_dockerfile mirage_dev_dockerfile with_ocaml_4_11_1 in
  let with_mirage_dev = Docker.build ~label:"mirage-dev" ~pull:false ~dockerfile:to_build (`Git src_mirage_dev)  (* Step 2: add mirage-dev*)
  in
  let to_build = apply_dockerfile mirage_skeleton_dockerfile with_mirage_dev in
  let with_mirage_skeleton = Docker.build ~label:"mirage-skeleton" ~pull:false ~dockerfile:to_build (`Git src_mirage_skeleton) (* Step 3: add mirage-skeleton. *)
  in
  let to_build = apply_dockerfile test_unikernel_dockerfile with_mirage_skeleton in
  let test_unikernel = Docker.build ~label:"unikernel tester" ~pull:false ~dockerfile:to_build (`Git src_mirage_ci) (* Step 4: add script. *)
  in
    [("tutorial/noop", "unix"); ("tutorial/noop", "hvt")]
  |> List.map (fun (unikernel,target) -> Docker.run ~label:(unikernel^"-"^target) ~run_args:["-v duniverse-cache:/home/opam/.cache/duniverse"] test_unikernel ~args:["/src/mirage-skeleton/"^unikernel; target])
  |> Current.all
