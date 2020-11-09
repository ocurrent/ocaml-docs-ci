module Git = Current_git
module Docker = Current_docker.Default

open Current.Syntax

let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()

let targets = ["unix"; "hvt"] (*; "xen"; "virtio"; "spt"; "muen"; "genode"]*)
let stages = [
    ("1: test-target", "tutorial", ["noop"]);
    ("2: tutorial", "tutorial", ["noop-functor"; "hello"; "hello-key"; "app_info"]);
    ("3: tutorial-lwt", "tutorial", ["lwt/echo_server"; "lwt/heads1"; "lwt/heads2";"lwt/timeout1";"lwt/timeout2"] );
    ("4: devices", "device-usage", ["block"; "clock"; "conduit_server"; "console"; "http-fetch"; "kv_ro"; "network"; "ping6"; "prng"; "tracing"]);
    ("5: applications", "applications", ["dhcp"; "dns"; "static_website_tls"])]


let ocaml_4_11_1 ~base =
  let open Dockerfile in 
  from (Docker.Image.hash base) @@
  run "opam repo remove beta" @@
  run "opam repo set-url default git+https://github.com/ocaml/opam-repository.git" @@
  run "opam switch create 4.11.1"
  

let solo5_bindings_pkgs = 
      ["hvt"] (* "xen"; "virtio"; "spt"; "muen"; "genode" *)
  |> List.map (fun x -> "solo5-bindings-"^x)
  |> String.concat " "

let mirage_dev_dockerfile ~base =
  let open Dockerfile in 
  from (Docker.Image.hash base) @@
  copy ~chown:"opam:opam" ~src:["."] ~dst:"/src/mirage-dev" () @@
  run "ls /src/mirage-dev/packages/" @@
  run "opam repo add dune-universe git+https://github.com/dune-universe/opam-overlays.git" @@
  run "opam repo add mirage-dev /src/mirage-dev" @@
  run "opam pin add -n git+https://github.com/ocamllabs/duniverse.git" @@
  run "opam depext ocaml-freestanding %s mirage opam-monorepo" solo5_bindings_pkgs @@
  run "opam install ocaml-freestanding" @@
  run "opam install %s mirage opam-monorepo" solo5_bindings_pkgs  

let mirage_skeleton_dockerfile ~base=
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  env [("OCAMLFIND_CONF", "/home/opam/.opam/4.11.1/lib/findlib.conf")] @@ (* TODO: Dunified findlib is not working correctly.*)
  run "sh -c \"echo '(lang dune 2.0)\n(cache enabled)\n' >> /home/opam/.config/dune/config\"" @@
  copy ~chown:"opam:opam" ~src:["."] ~dst:"/src/mirage-skeleton" ()

let test_unikernel_dockerfile ~base =
  let open Dockerfile in
  from (Docker.Image.hash base) @@
  run "mkdir -p /home/opam/.cache/duniverse/" @@
  volume "/home/opam/.cache/duniverse/" @@
  volume "/home/opam/.cache/dune/" @@
  copy ~chown:"opam:opam" ~src:["./scripts/unikernel_test.sh"] ~dst:"/" () @@ 
  entrypoint_exec ["/unikernel_test.sh"]

let apply_dockerfile fn image =
  let+ base = image in
  `Contents (fn ~base)

let build_test_image ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci () =
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
  Docker.build ~label:"unikernel tester" ~pull:false ~dockerfile:to_build (`Git src_mirage_ci) (* Step 4: add script. *)

let run_test ~test_image ~unikernel ~target =
  Docker.run ~label:(unikernel^"-"^target) 
    ~run_args:["-v"; "duniverse-cache:/home/opam/.cache/duniverse"; "-v"; "dune-cache:/home/opam/.cache/dune"] 
    ~args:["/src/mirage-skeleton/"^unikernel; target]
    test_image 

let test_target ~test_image ~stage ~unikernels ~target = 
  unikernels
  |> List.map (fun name -> run_test ~test_image ~unikernel:(stage^"/"^name) ~target)
  |> Current.all

let v ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci () =
  let test_image = build_test_image ~repo_mirage_skeleton ~repo_mirage_dev ~repo_mirage_ci () 
  in
  let rec aux test_image = function
    | [] -> Current.ignore_value test_image
    | (_, stage, unikernels)::[] -> 
        targets
        |> List.map (fun target -> test_target ~test_image ~stage ~unikernels ~target)
        |> Current.all
    | (_, stage, unikernels)::q ->
      let test_stage =
        targets
        |> List.map (fun target -> test_target ~test_image ~stage ~unikernels ~target)
        |> Current.all
      in
      aux (Current.gate ~on:test_stage test_image) q 
  in
  aux test_image stages
