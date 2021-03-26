module Git = Current_git

(* -------------------------- *)

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

let perform_solve ~job ~(platform : Platform.t) ~opam track =
  let open Lwt.Syntax in
  let package = Track.pkg track in
  let packages = [ OpamPackage.name_to_string package; "ocaml-base-compiler" ] in
  let constraints =
    [ (OpamPackage.name_to_string package, OpamPackage.version_to_string package) ]
  in
  let request =
    {
      Solver_api.Worker.Solve_request.opam_repository_commit =
        opam |> Current_git.Commit.id |> Current_git.Commit_id.hash;
      pkgs = packages;
      constraints;
      platforms =
        [
          ( "base",
            Solver_api.Worker.Vars.
              {
                arch = platform.arch |> Platform.arch_to_string;
                os = "linux";
                os_family = Platform.os_family platform.system.os;
                os_distribution = "linux";
                os_version = Platform.os_version platform.system.os;
              } );
        ];
    }
  in
  let switch = Current.Switch.create ~label:"solver switch" () in
  let* () = Current.Job.use_pool ~switch job pool in
  Capnp_rpc_lwt.Capability.with_ref (job_log job) @@ fun log ->
  let* res = Solver_api.Solver.solve solver request ~log in
  let+ () = Current.Switch.turn_off switch in
  match res with
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

(* ------------------------- *)
module SolverCache = struct
  type t = No_context

  let id = "incremental-solver"

  let pp f _ = Fmt.pf f "incremental solver"

  let auto_cancel = false

  let latched = true

  module Key = struct
    (* TODO: what happens when the platform changes / the blacklist. *)
    type t = { packages : Track.t list; blacklist : string list; platform : Platform.t }

    let digest { packages; blacklist; _ } =
      blacklist
      @ List.map (fun t -> (Track.pkg t |> OpamPackage.to_string) ^ "-" ^ Track.digest t) packages
      |> Digestif.SHA256.digestv_string |> Digestif.SHA256.to_hex
  end

  module Value = struct
    type t = Git.Commit.t

    let digest = Git.Commit.hash
  end

  module Outcome = struct
    type t = Package.t list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string

    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  type cache = (Track.t * Package.t) list [@@deriving yojson]

  let run No_context job Key.{ packages; blacklist; platform } opam =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Harmless job in
    let state_file = Fpath.(Current.state_dir id / "solved.json" |> to_string) in
    let cache_data =
      try
        match Yojson.Safe.from_file state_file |> cache_of_yojson with
        | Ok data -> data |> Track.Map.of_list
        | Error _ -> Track.Map.empty
      with Sys_error _ -> Track.Map.empty
    in
    let to_do = List.filter (fun x -> not (Track.Map.mem x cache_data)) packages in
    let* solved =
      Lwt_list.map_p
        (fun pkg ->
          let+ res = perform_solve ~job ~opam ~platform pkg in
          (pkg, res))
        to_do
    in
    let solved_success =
      List.filter_map
        (fun (track, result) ->
          let root = Track.pkg track in
          match result with
          | Ok (packages, commit) -> Some (track, Package.make ~blacklist ~commit ~root packages)
          | Error (`Msg msg) ->
              Current.Job.log job "Solving failed for %s: %s" (OpamPackage.to_string root) msg;
              None)
        solved
    in
    let cache_data =
      Track.Map.add_seq (List.to_seq solved_success) cache_data |> Track.Map.bindings
    in
    Current.Job.log job "Tracked: %d / Solved: %d / New: %d / Success: %d" (List.length packages)
      (List.length cache_data) (List.length solved) (List.length solved_success);
    let () = cache_data |> cache_to_yojson |> Yojson.Safe.to_file state_file in
    Lwt.return_ok (List.map snd cache_data)
end

module Solver = Current_cache.Generic (SolverCache)

let incremental ~(blacklist : string list) ~(opam : Git.Commit.t Current.t)
    (packages : Track.t list Current.t) : Package.t list Current.t =
  let open Current.Syntax in
  Current.component "incremental solver"
  |> let> opam = opam and> packages = packages in
     Solver.run No_context { packages; blacklist; platform = Platform.platform_amd64 } opam
