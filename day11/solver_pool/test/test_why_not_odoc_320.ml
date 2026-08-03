(** For a single [+ox] target that currently pulls [odoc.3.1.0+ox], ask the
    solver to ALSO pin [odoc.3.2.0+ox] and report what conflict prevented
    [3.2.0+ox] in the first place. *)

let repos =
  [
    "/home/jjl25/ocaml/opam-repository";
    "/home/jjl25/oxcaml/opam-repository";
    "/home/jjl25/local/opam-repository";
  ]

let ocaml_version = OpamPackage.of_string "ocaml-variants.5.2.0+ox"

let target =
  OpamPackage.of_string
    (match Sys.argv with [| _; t |] -> t | _ -> "cstruct.3.3.0")

let extra_targets = [ OpamPackage.of_string "odoc.3.2.0+ox" ]

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let repos_with_shas =
    List.map
      (fun path ->
        let sha =
          let cmd = Bos.Cmd.(v "git" % "-C" % path % "rev-parse" % "HEAD") in
          match Bos.OS.Cmd.(run_out cmd |> out_string) with
          | Ok (s, (_, `Exited 0)) -> String.trim s
          | _ -> "HEAD"
        in
        (path, sha))
      repos
  in
  Printf.printf "Target: %s (extra-target: odoc.3.2.0+ox)\n"
    (OpamPackage.to_string target);
  let env = (env :> Eio_unix.Stdenv.base) in
  let results =
    Day11_solver_pool.Solver_pool.solve_many ~sw env ~ocaml_version
      ~extra_targets ~np:1 ~repos:repos_with_shas [ target ]
  in
  match List.assoc_opt target results with
  | None -> Printf.printf "no result\n"
  | Some (Ok _) -> Printf.printf "OK — solved with odoc.3.2.0+ox pinned\n"
  | Some (Error (msg, _)) -> Printf.printf "FAILED:\n%s\n" msg
