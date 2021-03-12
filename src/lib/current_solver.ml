module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)

let pool = Current.Pool.create ~label:"solver" 4

module Op = struct
  type t = No_context

  module Key = struct
    type t = {
      repo : Current_git.Commit.t;
      packages : string list;
      system : Platform.system;
      constraints : (OpamParserTypes.relop * OpamTypes.version) OpamTypes.name_map;
    }

    let digest { packages; system; constraints; _ } =
      let json =
        `Assoc
          [
            (*("repo", `String (Current_git.Commit.hash repo)); we omit repo the key. once a solve is successful,
              we should keep the universe. *)
            ("packages", `List (List.map (fun p -> `String p) packages));
            ("system", `String (Fmt.str "%a" Platform.pp_system system));
            ( "constraints",
              `String
                (OpamPackage.Name.Map.to_string
                   (fun (_, version) -> OpamPackage.Version.to_string version)
                   constraints) );
          ]
      in
      Yojson.to_string json
  end

  module Value = struct
    type t = O.OpamPackage.t list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let auto_cancel = true

  let id = "mirage-ci-solver"

  let pp f _ = Fmt.string f "Opam solver"

  open Lwt.Syntax

  let env ~(system : Platform.system) =
    Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"linux"
      ~os_version:(Platform.os_version system.os) ~os_family:(Platform.os_family system.os) ()

  let build No_context job { Key.repo; packages; system; constraints } =
    let* () = Current.Job.start ~pool ~level:Harmless job in
    Current_git.with_checkout ~job repo @@ fun dir ->
    let dir = Fpath.(to_string (dir / "packages")) in
    let solver_context =
      Opam_0install.Dir_context.create ~constraints ~env:(env ~system)
        ~test:OpamPackage.Name.Set.empty dir
    in
    let t0 = Unix.gettimeofday () in
    let r = Solver.solve solver_context (packages |> List.map OpamPackage.Name.of_string) in
    let t1 = Unix.gettimeofday () in
    match r with
    | Ok sels ->
        let pkgs = Solver.packages_of_result sels in
        Current.Job.log job "Solver succeeded! (%.2fs)" (t1 -. t0);
        List.iter
          (fun pk ->
            let name = OpamPackage.name pk |> OpamPackage.Name.to_string in
            let version = OpamPackage.version pk |> OpamPackage.Version.to_string in
            Current.Job.log job "> %s=%s " name version)
          pkgs;
        Lwt.return_ok pkgs
    | Error diagnostics -> Lwt.return (Error (`Msg (Solver.diagnostics diagnostics)))
end

module Solver_cache = Current_cache.Make (Op)

let v ~system ~repo ~packages ~constraints =
  let open Current.Syntax in
  Current.component "solver"
  |> let> repo = repo and> constraints = constraints and> packages = packages in
     Solver_cache.get No_context { system; repo; packages; constraints }

(*
   ( constraints |> OpamPackage.Name.Map.bindings
   |> List.map (fun (name, c) -> OpamFormula.string_of_atom (name, Some c))
   |> String.concat ", " )
*)
