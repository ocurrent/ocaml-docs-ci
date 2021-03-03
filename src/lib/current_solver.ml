type resolution = { name : string; version : string; opamfile : Opamfile.t } [@@deriving yojson]

module Solver = Opam_0install.Solver.Make (Dirs_context)

module Op = struct
  type t = No_context

  module Key = struct
    type t = {
      repos : (string * Current_git.Commit.t) list;
      packages : string list;
      system : Platform.system;
    }

    let digest { repos; packages; system } =
      let json =
        `Assoc
          [
            ( "repos",
              `List (List.map (fun (_, commit) -> `String (Current_git.Commit.hash commit)) repos)
            );
            ("packages", `List (List.map (fun p -> `String p) packages));
            ("system", `String (Fmt.str "%a" Platform.pp_system system));
          ]
      in
      Yojson.to_string json
  end

  module Value = struct
    type t = resolution list [@@deriving yojson]

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

  let with_checkouts ~job commits fn =
    let rec aux acc = function
      | [] -> fn (List.rev acc)
      | commit :: next ->
          Current_git.with_checkout ~job commit (fun tmpdir -> aux (tmpdir :: acc) next)
    in
    aux [] commits

  let build No_context job { Key.repos; packages; system } =
    let* () = Current.Job.start ~level:Harmless job in
    let repos = List.map snd repos in
    with_checkouts ~job repos @@ fun dirs ->
    let ocaml_package = OpamPackage.Name.of_string "ocaml" in
    let ocaml_version =
      OpamPackage.Version.of_string (Fmt.str "%a" Platform.pp_exact_ocaml system.ocaml)
    in
    let dirs = List.map (fun dir -> Fpath.(to_string (dir / "packages"))) dirs in
    let solver_context =
      Dirs_context.create
        ~constraints:(OpamPackage.Name.Map.singleton ocaml_package (`Eq, ocaml_version))
        ~env:(env ~system) ~test:OpamPackage.Name.Set.empty dirs
    in
    let t0 = Unix.gettimeofday () in
    let r =
      Solver.solve solver_context
        (ocaml_package :: (packages |> List.map OpamPackage.Name.of_string))
    in
    let t1 = Unix.gettimeofday () in
    Printf.printf "%.2f\n" (t1 -. t0);
    match r with
    | Ok sels ->
        let pkgs = Solver.packages_of_result sels in
        Lwt.return_ok
          (List.map
             (fun pk ->
               {
                 name = OpamPackage.name pk |> OpamPackage.Name.to_string;
                 version = OpamPackage.version pk |> OpamPackage.Version.to_string;
                 opamfile =
                   Dirs_context.get_opamfile solver_context pk
                   |> OpamFile.OPAM.write_to_string |> Opamfile.unmarshal
                   (* hmm *);
               })
             pkgs)
    | Error diagnostics -> Lwt.return (Error (`Msg (Solver.diagnostics diagnostics)))
end

module Solver_cache = Current_cache.Make (Op)

let v ~system ~repos ~packages =
  let open Current.Syntax in
  Current.component "solver (%s)" (String.concat "," packages)
  |> let> repos = repos in
     Solver_cache.get No_context { system; repos; packages }
