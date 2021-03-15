module Worker = Solver_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store
open Lwt.Infix

let env (vars : Worker.Vars.t) =
  Opam_0install.Dir_context.std_env ~arch:vars.arch ~os:vars.os
    ~os_distribution:vars.os_distribution ~os_version:vars.os_version ~os_family:vars.os_family ()

let solve ~packages ~constraints ~root_pkgs (vars : Worker.Vars.t) =
  let context =
    Git_context.create () ~packages ~env:(env vars) ~constraints
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context root_pkgs in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels ->
      let pkgs = Solver.packages_of_result sels in
      Ok (List.map OpamPackage.to_string pkgs)
  | Error diagnostics -> Error (Solver.diagnostics diagnostics)

let main commit =
  let packages =
    Lwt_main.run
      (Opam_repository.open_store () >>= fun store -> Git_context.read_packages store commit)
  in
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> ()
    | len ->
        let len = int_of_string len in
        let data = really_input_string stdin len in
        let request =
          Worker.Solve_request.of_yojson (Yojson.Safe.from_string data) |> Result.get_ok
        in
        let { Worker.Solve_request.opam_repository_commit; pkgs; constraints; platforms } =
          request
        in
        let opam_repository_commit = Store.Hash.of_hex opam_repository_commit in
        assert (Store.Hash.equal opam_repository_commit commit);
        let root_pkgs = pkgs |> List.map OpamPackage.Name.of_string in
        let constraints =
          constraints
          |> List.map (fun (name, version) ->
                 (OpamPackage.Name.of_string name, (`Eq, OpamPackage.Version.of_string version)))
          |> OpamPackage.Name.Map.of_list
        in
        platforms
        |> List.iter (fun (_id, platform) ->
               let msg =
                 match solve ~packages ~constraints ~root_pkgs platform with
                 | Ok packages -> "+" ^ String.concat " " packages
                 | Error msg -> "-" ^ msg
               in
               Printf.printf "%d\n%s%!" (String.length msg) msg);
        aux ()
  in
  aux ()

let main commit =
  try main commit
  with ex ->
    Fmt.epr "solver bug: %a@." Fmt.exn ex;
    let msg = match ex with Failure msg -> msg | ex -> Printexc.to_string ex in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex
