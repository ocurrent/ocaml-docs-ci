(** Empirically test whether each [+ox] variant the solver picks for a mainline
    driver solve actually compiles under the mainline compiler.

    For each candidate, attempt a tool build pinned to
    [ocaml-base-compiler.5.4.1]. Report OK / FAILED per package.

    Run via: dune exec day11/solver_pool/test/test_ox_variants.exe *)

(* Inconclusive candidates from the first pass — their own build step
   never ran because a broken [+ox] dep failed earlier in the chain.
   Retry with the 4 confirmed-broken [+ox] variants pinned to their
   mainline versions, so the chain actually reaches each candidate's
   own compile step. *)
let candidates =
  [
    "eio.1.3+ox";
    "eio_linux.1.3+ox";
    "eio_main.1.3+ox";
    "eio_posix.1.3+ox";
    "uutf.1.0.4+ox";
  ]

(* Pin the 5 confirmed-oxcaml-only deps to their mainline equivalents. *)
let constraints =
  List.map OpamPackage.of_string
    [
      "ocaml-compiler-libs.v0.17.0";
      "ocamlbuild.0.16.1";
      "ocamlfind.1.9.8";
      "re.1.14.0";
      "topkg.1.1.1";
    ]

let mainline = OpamPackage.of_string "ocaml-base-compiler.5.4.1"
let profile_name = "oxcaml"

let () =
  let profile_dir = Day11_batch.Profile.default_dir () in
  let profile =
    match
      Day11_batch.Profile.load
        ~dir:Fpath.(profile_dir / "profiles")
        ~name:profile_name
    with
    | Error (`Msg e) ->
        Printf.eprintf "Profile.load: %s\n" e;
        exit 1
    | Ok p -> p
  in
  let cache_dir = Fpath.(profile_dir / "cache") in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let env = (env :> Eio_unix.Stdenv.base) in
  let ctx = Day11_batch.Profile_ctx.load profile ~cache_dir in
  let ctx =
    match Day11_batch.Profile_ctx.ensure_base ~sw env ctx with
    | Ok c -> c
    | Error (`Msg e) ->
        Printf.eprintf "ensure_base: %s\n%!" e;
        exit 1
  in
  let benv = ctx.benv in
  let results =
    List.map
      (fun s ->
        let target = OpamPackage.of_string s in
        Printf.printf "\n================ %s ================\n%!" s;
        let r =
          try
            Day11_opam_build.Tools.build_tool ~sw env benv ~np:8
              ~packages:ctx.git_packages ~repos:ctx.repos_with_shas ~constraints
              ~ocaml_version:mainline ~doc:false target
          with exn ->
            Rresult.R.error_msgf "exception: %s" (Printexc.to_string exn)
        in
        match r with
        | Ok _ ->
            Printf.printf "RESULT: %s OK\n%!" s;
            (s, `Ok)
        | Error (`Msg e) ->
            Printf.printf "RESULT: %s FAILED: %s\n%!" s e;
            (s, `Failed e))
      candidates
  in
  Printf.printf "\n================ summary ================\n";
  let ok = List.filter (fun (_, r) -> r = `Ok) results
  and fail = List.filter (fun (_, r) -> r <> `Ok) results in
  Printf.printf "OK (%d):\n" (List.length ok);
  List.iter (fun (s, _) -> Printf.printf "  %s\n" s) ok;
  Printf.printf "FAILED (%d):\n" (List.length fail);
  List.iter
    (function s, `Failed e -> Printf.printf "  %s — %s\n" s e | _ -> ())
    fail
