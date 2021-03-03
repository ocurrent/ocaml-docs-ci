open Mirage_ci_lib
open Current.Syntax

let targets = [ "unix"; "hvt"; "xen" ] (* "virtio"; "spt"; "muen" ]*)

let stages =
  [
    ("1: test-target", "tutorial", [ "noop" ]);
    ("2: tutorial", "tutorial", [ "noop-functor"; "hello"; "hello-key"; "app_info" ]);
    ( "3: tutorial-lwt",
      "tutorial",
      [ "lwt/echo_server"; "lwt/heads1"; "lwt/heads2"; "lwt/timeout1"; "lwt/timeout2" ] );
    ( "4: devices",
      "device-usage",
      [
        "block";
        "clock";
        "conduit_server";
        "console";
        "http-fetch";
        "kv_ro";
        "network";
        "pgx";
        "ping6";
        "prng";
      ] );
    (* removed tracing: not supported anywhere. *)
    ("5: applications", "applications", [ "dhcp"; "dns"; "static_website_tls" ]);
  ]

(* muen: no support for block *)
let overrides = [ ("block", targets |> List.filter (( <> ) "muen")) ]

type configuration_4 = {
  mirage : Mirage.t Current.t;
  monorepo : Mirage_ci_lib.Monorepo.t Current.t;
  repos : Repository.t list Current.t;
  skeleton : Current_git.Commit.t Current.t;
}

type test = { platform : Platform.t; unikernel : string; target : string }

let run_test_mirage_4 { unikernel; platform; target } configuration =
  let c = configuration in
  let base =
    let+ repos = c.repos in
    Platform.spec platform.system |> Spec.add (Setup.add_repositories repos)
  in
  let configuration = Mirage.configure ~project:c.skeleton ~unikernel ~target c.mirage in
  let base =
    let+ base = base in
    (* pre-install ocaml-freestanding *)
    Spec.add (Setup.install_tools [ "ocaml-freestanding" ]) base
  in
  let skeleton =
    (* add a fake dep to the lockfile (only rebuild if lockfile changed.)*)
    let+ _ =
      Monorepo.lock
        ~value:("mirage-" ^ unikernel ^ "-" ^ target)
        ~repos:c.repos ~opam:configuration c.monorepo
    and+ skeleton = c.skeleton in
    Current_git.Commit.id skeleton
  in
  Mirage.build ~platform ~base ~project:skeleton ~unikernel ~target ()
  |> Current.collapse ~key:("Unikernel " ^ unikernel ^ "@" ^ target) ~value:"" ~input:c.repos

type configuration_main = {
  mirage : Current_git.Commit_id.t Current.t;
  repos : Repository.t list Current.t;
  skeleton : Current_git.Commit_id.t Current.t;
}

let run_test_mirage_main { unikernel; platform; target } configuration =
  let c = configuration in
  let base =
    let+ repos = c.repos in
    Platform.spec platform.system |> Spec.add (Setup.add_repositories repos)
  in
  let base =
    let+ base = base and+ mirage = c.mirage in
    (* pre-install ocaml-freestanding *)
    Spec.add (Setup.install_tools [ "ocaml-freestanding" ]) base
    |> Spec.add
         [ Obuilder_spec.run ~network:Setup.network "opam pin -n -y %s" (Setup.remote_uri mirage) ]
  in
  Mirage.build ~platform ~cmd:"mirage build" ~base ~project:c.skeleton ~unikernel ~target ()
  |> Current.collapse ~key:("Unikernel " ^ unikernel ^ "@" ^ target) ~value:"" ~input:c.repos

let test_stage ~stage ~unikernels ~target ~platform ~run_test configuration =
  unikernels
  |> List.filter (fun name ->
         overrides
         |> List.find_map (fun (n, t) -> if n = name then Some t else None)
         |> Option.map (List.mem target)
         |> Option.value ~default:true)
  |> List.map (fun name ->
         run_test { unikernel = stage ^ "/" ^ name; target; platform } configuration)
  |> Current.all

let multi_stage_test ~platform ~targets ~configure ~run_test mirage_skeleton =
  let rec aux ~target skeleton = function
    | [] -> skeleton |> Current.ignore_value
    | (name, stage, unikernels) :: q ->
        let configuration = configure skeleton in
        let test_stage =
          test_stage ~run_test ~stage ~unikernels ~target ~platform configuration
          |> Current.collapse ~key:("Test stage " ^ name) ~value:"" ~input:skeleton
        in
        let mirage_skeleton =
          let+ _ = test_stage and+ skeleton = skeleton in
          skeleton
        in
        aux ~target mirage_skeleton q
  in
  List.map (fun target -> (target, aux ~target mirage_skeleton stages)) targets
  |> Current.all_labelled

(* MIRAGE 4 TEST *)

let v_4 ~repos ~monorepo ~(platform : Platform.t) ~targets mirage_skeleton =
  let mirage = Mirage.v ~system:platform.system ~repos in
  multi_stage_test ~platform ~targets ~run_test:run_test_mirage_4
    ~configure:(fun skeleton -> { mirage; monorepo; repos; skeleton })
    mirage_skeleton

(* MIRAGE MAIN TEST *)

let v_main ~platform ~mirage ~repos mirage_skeleton =
  multi_stage_test ~platform ~targets ~run_test:run_test_mirage_main
    ~configure:(fun skeleton -> { mirage; repos; skeleton })
    mirage_skeleton
