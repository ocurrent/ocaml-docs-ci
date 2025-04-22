module Worker = Solver_api.Worker
module Solver = Opam_0install.Solver.Make (Git_context)
module Store = Git_unix.Store
open Lwt.Infix

let env (vars : Worker.Vars.t) =
  let env =
    Opam_0install.Dir_context.std_env ~arch:vars.arch ~os:vars.os
      ~os_distribution:vars.os_distribution ~os_version:vars.os_version
      ~os_family:vars.os_family ()
  in
  function "opam-version" -> Some (OpamTypes.S "2.1.0") | v -> env v

let get_names = OpamFormula.fold_left (fun a (name, _) -> name :: a) []

let universes ~packages (resolutions : OpamPackage.t list) =
  let aux root =
    let name, version = (OpamPackage.name root, OpamPackage.version root) in
    let opamfile : OpamFile.OPAM.t =
      packages
      |> OpamPackage.Name.Map.find name
      |> OpamPackage.Version.Map.find version
    in
    let deps =
      opamfile
      |> OpamFile.OPAM.depends
      |> OpamFilter.partial_filter_formula
           (OpamFilter.deps_var_env ~build:true ~post:false ~test:false
              ~doc:false ~dev_setup:false ~dev:false)
      |> get_names
      |> OpamPackage.Name.Set.of_list
    in
    let depopts =
      opamfile
      |> OpamFile.OPAM.depopts
      |> OpamFilter.partial_filter_formula
           (OpamFilter.deps_var_env ~build:true ~post:false ~test:false
              ~doc:false ~dev_setup:false ~dev:false)
      |> get_names
      |> OpamPackage.Name.Set.of_list
    in
    let all_deps = OpamPackage.Name.Set.union deps depopts in
    let deps =
      resolutions
      |> List.filter (fun res ->
             let name = OpamPackage.name res in
             OpamPackage.Name.Set.mem name all_deps)
    in
    let result = OpamPackage.Set.of_list deps in
    result
  in
  let simple_deps =
    List.fold_left
      (fun acc pkg -> OpamPackage.Map.add pkg (aux pkg) acc)
      OpamPackage.Map.empty resolutions
  in

  let rec closure pkgs =
    let deps =
      List.map
        (fun pkg -> OpamPackage.Map.find pkg simple_deps)
        (OpamPackage.Set.to_list pkgs)
    in
    let all = List.fold_left OpamPackage.Set.union OpamPackage.Set.empty deps in
    let new_deps = OpamPackage.Set.diff all pkgs in
    if OpamPackage.Set.is_empty new_deps then pkgs
    else closure (OpamPackage.Set.union pkgs all)
  in

  List.rev_map
    (fun pkg ->
      let name, version = (OpamPackage.name pkg, OpamPackage.version pkg) in
      let opamfile : OpamFile.OPAM.t =
        packages
        |> OpamPackage.Name.Map.find name
        |> OpamPackage.Version.Map.find version
      in
      let str = OpamFile.OPAM.write_to_string opamfile in
      ( pkg,
        str,
        closure (OpamPackage.Map.find pkg simple_deps)
        |> OpamPackage.Set.elements ))
    resolutions

type solve_result = Worker.solve_result = {
  compile_universes : (string * string * string list) list;
  link_universes : (string * string * string list) list;
}
[@@deriving yojson]

let solve ~packages ~constraints ~root_pkgs (vars : Worker.Vars.t) =
  let extended = Git_context.extend_packages packages in
  let context =
    Git_context.create () ~packages:extended ~env:(env vars) ~constraints
  in
  let t0 = Unix.gettimeofday () in
  let r = Solver.solve context root_pkgs in
  let t1 = Unix.gettimeofday () in
  Printf.printf "%.2f\n" (t1 -. t0);
  match r with
  | Ok sels ->
      Format.eprintf "Got sels\n%!";
      let pkgs = Solver.packages_of_result sels in
      Format.eprintf "Got pkgs\n%!";
      let compile_universes = universes ~packages pkgs in
      Format.eprintf "Got universes\n%!";
      let link_universes = universes ~packages:extended pkgs in
      let map_universes univs =
        List.map
          (fun (pkg, str, univ) ->
            (OpamPackage.to_string pkg, str, List.map OpamPackage.to_string univ))
          univs
      in
      Ok
        {
          compile_universes = map_universes compile_universes;
          link_universes = map_universes link_universes;
        }
  | Error diagnostics -> Error (Solver.diagnostics diagnostics)

let test commit =
  Format.eprintf "Running test\n%!";
  let packages =
    Lwt_main.run
      ( Opam_repository.open_store () >>= fun store ->
        Git_context.read_packages store commit )
  in
  let root_pkgs =
    List.map OpamPackage.Name.of_string
      [ "ocaml"; "ocaml-base-compiler"; "vpnkit" ]
  in
  let constraints =
    List.map
      (fun (name, rel, version) ->
        ( OpamPackage.Name.of_string name,
          (rel, OpamPackage.Version.of_string version) ))
      [
        ("ocaml-base-compiler", `Geq, "4.08.0");
        ("ocaml", `Leq, "5.2.0");
        ("vpnkit", `Eq, "0.2.0");
      ]
    |> OpamPackage.Name.Map.of_list
  in
  let platform =
    {
      Worker.Vars.arch = "x86_64";
      os = "linux";
      os_distribution = "linux";
      os_family = "ubuntu";
      os_version = "20.04";
    }
  in
  Format.eprintf "Calling solve\n%!";
  match solve ~packages ~constraints ~root_pkgs platform with
  | Ok packages ->
      Printf.printf "%s\n"
        (solve_result_to_yojson packages |> Yojson.Safe.to_string)
  | Error msg -> Printf.printf "%s\n" msg

let main commit =
  let packages =
    Lwt_main.run
      ( Opam_repository.open_store () >>= fun store ->
        Git_context.read_packages store commit )
  in
  let rec aux () =
    match input_line stdin with
    | exception End_of_file -> ()
    | len ->
        let len = int_of_string len in
        let data = really_input_string stdin len in
        let request =
          Worker.Solve_request.of_yojson (Yojson.Safe.from_string data)
          |> Result.get_ok
        in
        let {
          Worker.Solve_request.opam_repository_commit;
          pkgs;
          constraints;
          platforms;
        } =
          request
        in
        let opam_repository_commit = Store.Hash.of_hex opam_repository_commit in
        assert (Store.Hash.equal opam_repository_commit commit);
        let root_pkgs = pkgs |> List.rev_map OpamPackage.Name.of_string in
        let constraints =
          constraints
          |> List.rev_map (fun (name, rel, version) ->
                 ( OpamPackage.Name.of_string name,
                   (rel, OpamPackage.Version.of_string version) ))
          |> OpamPackage.Name.Map.of_list
        in
        platforms
        |> List.iter (fun (_id, platform) ->
               let msg =
                 match solve ~packages ~constraints ~root_pkgs platform with
                 | Ok packages ->
                     "+"
                     ^ (solve_result_to_yojson packages |> Yojson.Safe.to_string)
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
    let msg =
      match ex with Failure msg -> msg | ex -> Printexc.to_string ex
    in
    let msg = "!" ^ msg in
    Printf.printf "0.0\n%d\n%s%!" (String.length msg) msg;
    raise ex
