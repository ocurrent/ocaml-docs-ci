open Mirage_ci_lib
open Current.Syntax

let targets = [ "unix"; "hvt"; ] (*"xen"; "virtio"; "spt"; "muen" ]*)

let stages =
  [
    ("1: test-target", "tutorial", [ "noop" ]);
    ("2: tutorial", "tutorial", [ "noop-functor"; "hello"; "hello-key"; "app_info" ]);
    ]
   (* ( "3: tutorial-lwt",
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
  ]*)

(* muen: no support for block *)
let overrides = [ ("block", targets |> List.filter (( <> ) "muen")) ]

let solo5_bindings_pkgs =
  [ "hvt"; "xen"; "virtio"; "spt"; "muen" ]
  |> List.map (fun x -> "solo5-bindings-" ^ x)

let run_test ~repos ~skeleton ~unikernel ~target =
  let base = 
    let+ repos = repos in
    Spec.make "ocaml/opam:ubuntu-ocaml-4.11" |> Spec.add (Setup.add_repositories repos) 
  in
  let configuration = Mirage.configure ~base ~project:skeleton ~unikernel ~target in
  let base = 
    let+ base = base in (* pre-install ocaml-freestanding *)
    Spec.add (Setup.install_tools ["ocaml-freestanding"]) base
  in
  let skeleton =
    let+ _ = Monorepo.lock ~base ~opam:configuration  (* add a fake dep to the lockfile (only rebuild if lockfile changed.)*)
    and+ skeleton = skeleton in 
    skeleton
  in 
  Mirage.build ~base ~project:skeleton ~unikernel ~target

let test_stage ~repos ~skeleton ~stage ~unikernels ~target =
  unikernels
  |> List.filter (fun name ->
         overrides
         |> List.find_map (fun (n, t) -> if n = name then Some t else None)
         |> Option.map (List.mem target)
         |> Option.value ~default:true)
  |> List.map (fun name -> run_test ~repos ~skeleton ~unikernel:(stage ^ "/" ^ name) ~target)
  |> Current.all


let remote_uri commit =
  let commit_id = Current_git.Commit.id commit in
  let repo = Current_git.Commit_id.repo commit_id in
  let commit = Current_git.Commit.hash commit in
  repo ^ "#" ^ commit
  
let v ~repos mirage_skeleton =
  let repos = Current.list_seq repos in
  Current.with_context repos (fun () -> 
    let repos =
      let+repos = repos in
      List.map (fun (name, commit) -> (name, remote_uri commit)) repos 
    in 
    let rec aux ~target gate = 
      let mirage_skeleton = (* create some fake dependency on the gate *)
        let+ _ = gate
        and+ s = mirage_skeleton in s 
      in 
      function
      | [] -> gate
      | [ (_, stage, unikernels) ] -> test_stage ~repos ~skeleton:mirage_skeleton ~stage ~unikernels ~target
      | (_, stage, unikernels) :: q ->
          let test_stage = test_stage ~repos ~skeleton:mirage_skeleton ~stage ~unikernels ~target in
          aux ~target (Current.gate ~on:test_stage gate) q
    in
    List.map (fun target -> (target, aux ~target (Current.return ()) stages)) targets |> Current.all_labelled
  )
