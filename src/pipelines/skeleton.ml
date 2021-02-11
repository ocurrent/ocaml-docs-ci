open Mirage_ci_lib
open Current.Syntax

let targets = [ "unix"; "hvt" ] (*"xen"; "virtio"; "spt"; "muen" ]*)

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

let solo5_bindings_pkgs =
  [ "hvt"; "xen"; "virtio"; "spt"; "muen" ] |> List.map (fun x -> "solo5-bindings-" ^ x)

let run_test ~mirage ~monorepo ~repos ~skeleton ~unikernel ~target =
  let base =
    let+ repos = repos in
    Spec.make "ocaml/opam:ubuntu-ocaml-4.11" |> Spec.add (Setup.add_repositories repos)
  in
  let configuration = Mirage.configure ~project:skeleton ~unikernel ~target mirage in
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
        ~repos ~opam:configuration monorepo
    and+ skeleton = skeleton in
    skeleton
  in
  Mirage.build ~base ~project:skeleton ~unikernel ~target

let test_stage ~mirage ~monorepo ~repos ~name ~skeleton ~stage ~unikernels ~target =
  unikernels
  |> List.filter (fun name ->
         overrides
         |> List.find_map (fun (n, t) -> if n = name then Some t else None)
         |> Option.map (List.mem target)
         |> Option.value ~default:true)
  |> List.map (fun name ->
         run_test ~repos ~mirage ~monorepo ~skeleton ~unikernel:(stage ^ "/" ^ name) ~target)
  |> Current.all
  |> Current.collapse ~key:("Test stage "^name) ~value:"" ~input:skeleton

let v ~repos ~monorepo mirage_skeleton =
  let mirage = Mirage.v ~repos in
  let rec aux ~target skeleton =
    function
    | [] -> skeleton |> Current.ignore_value
    | (name, stage, unikernels) :: q ->
        let test_stage =
          test_stage ~mirage ~monorepo ~repos ~name ~skeleton ~stage ~unikernels
            ~target
        in
        let mirage_skeleton =
          let+ _ = test_stage 
          and+ skeleton = skeleton
          in skeleton 
        in
        aux ~target mirage_skeleton q
  in
  List.map (fun target -> (target, aux ~target mirage_skeleton stages)) targets
  |> Current.all_labelled
