module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)

type commit = string [@@deriving yojson]

let solver = Solver_pool.spawn_local ()

let job_log job =
  let module X = Solver_api.Raw.Service.Log in
  X.local
  @@ object
       inherit X.service

       method write_impl params release_param_caps =
         let open X.Write in
         release_param_caps ();
         let msg = Params.msg_get params in
         Current.Job.write job msg;
         Capnp_rpc_lwt.Service.(return (Response.create_empty ()))
     end

let pool = Current.Pool.create ~label:"solver" 8

module Op = struct
  type t = No_context

  module Key = struct
    type t = {
      repo : Current_git.Commit.t;
      packages : string list;
      system : Platform.system;
      constraints : (string * string) list;
    }

    let digest { packages; system; constraints; repo } =
      let json =
        `Assoc
          [
            ("repo", `String (Current_git.Commit.hash repo));
            ("packages", `List (List.map (fun p -> `String p) packages));
            ("system", `String (Fmt.str "%a" Platform.pp_system system));
            ("constraints", `List (List.map (fun (a, b) -> `String (a ^ "=" ^ b)) constraints));
          ]
      in
      Yojson.to_string json
  end

  module Value = struct
    type t = (O.OpamPackage.t * O.OpamPackage.t list) list * commit [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let auto_cancel = true

  let id = "mirage-ci-solver"

  let pp f _ = Fmt.string f "Opam solver"

  open Lwt.Syntax

  let build No_context job { Key.repo; packages; system; constraints } =
    let open Lwt.Infix in
    let* () = Current.Job.start ~pool ~level:Harmless job in
    let request =
      {
        Solver_api.Worker.Solve_request.opam_repository_commit =
          repo |> Current_git.Commit.id |> Current_git.Commit_id.hash;
        pkgs = packages;
        constraints;
        platforms =
          [
            ( "base",
              Solver_api.Worker.Vars.
                {
                  arch = "arm64";
                  os = "linux";
                  os_family = Platform.os_family system.os;
                  os_distribution = "linux";
                  os_version = Platform.os_version system.os;
                } );
          ];
      }
    in
    Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
    Solver_api.Solver.solve solver request ~log >|= function
    | Ok [] -> Fmt.error_msg "no platform"
    | Ok [ x ] ->
        Ok
          ( List.map
              (fun (a, b) ->
                Current.Job.log job "%s: %s" a (String.concat "; " b);
                (OpamPackage.of_string a, List.map OpamPackage.of_string b))
              x.packages,
            x.commit )
    | Ok _ -> Fmt.error_msg "??"
    | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg
end

module Solver_cache = Misc.LatchedBuilder (Op)

let v ~system ~repo ~packages ~constraints =
  let open Current.Syntax in
  Current.component "solver"
  |> let> repo = repo and> constraints = constraints and> packages = packages in
     Solver_cache.get No_context { system; repo; packages; constraints }
