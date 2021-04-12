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

let pool = Current.Pool.create ~label:"solver" Config.jobs

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
    let solution = List.map
      (fun (a, b) ->
        Current.Job.log job "%s: %s" a (String.concat "; " b);
        (OpamPackage.of_string a, List.map OpamPackage.of_string b))
      x.packages
    in
    let min_compiler_version = OpamPackage.Version.of_string "4.02.3" in
    let compiler = List.find (fun (p,_) -> OpamPackage.name p |> OpamPackage.Name.to_string = "ocaml-base-compiler") solution |> fst in
    let version = OpamPackage.version compiler in
    if OpamPackage.Version.compare version min_compiler_version >= 0
    then
      Ok
        ( solution,
          x.commit )
  else
    Fmt.error_msg "Solution requires compiler verion older than minimum"
  | Ok _ -> Fmt.error_msg "??"
  | Error (`Msg msg) -> Fmt.error_msg "Error from solver: %s" msg

module Cache = struct
  let id = "solver-cache"

  type cache_value = Package.t option

  let fname track =
    let digest = Track.digest track in
    let name = Track.pkg track |> OpamPackage.name_to_string in
    let name_version = Track.pkg track |> OpamPackage.version_to_string in
    Fpath.(Current.state_dir id / name / name_version / digest)

  let mem track =
    let fname = fname track in
    match Bos.OS.Path.exists fname with Ok true -> true | _ -> false

  let read track : cache_value option =
    let fname = fname track in
    try
      let file = open_in (Fpath.to_string fname) in
      let result = Marshal.from_channel file in
      close_in file;
      Some result
    with Failure _ -> None

  let write ((track, value) : Track.t * cache_value) =
    let fname = fname track in
    let _ = Bos.OS.Dir.create (fst (Fpath.split_base fname)) |> Result.get_ok in
    let file = open_out (Fpath.to_string fname) in
    Marshal.to_channel file value [];
    close_out file
end

type key = Track.t

type t = Track.t list

let keys t = t

let get key = Cache.read key |> Option.get (* is in cache ? *) |> Option.get

(* is solved ? *)

(* ------------------------- *)
module Solver = struct
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
    type t = Track.t list

    let marshal t = Marshal.to_string t []

    let unmarshal t = Marshal.from_string t 0
  end

  let run No_context job Key.{ packages; blacklist; platform } opam =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Harmless job in
    let to_do = List.filter (fun x -> not (Cache.mem x)) packages in
    let* solved =
      Lwt_list.map_p
        (fun pkg ->
          let+ res = perform_solve ~job ~opam ~platform pkg in
          let root = Track.pkg pkg in
          let result =
            match res with
            | Ok (packages, commit) -> Some (Package.make ~blacklist ~commit ~root packages)
            | Error (`Msg msg) ->
                Current.Job.log job "Solving failed for %s: %s" (OpamPackage.to_string root) msg;
                None
          in
          Cache.write (pkg, result);
          Option.is_some result)
        to_do
    in
    Current.Job.log job "Solved: %d / New: %d / Success: %d" (List.length packages)
      (List.length solved)
      (List.length (solved |> List.filter (fun x -> x)));
    Lwt.return_ok
      ( packages
      |> List.filter (fun x -> match Cache.read x with Some (Some _) -> true | _ -> false) )
end

module SolverCache = Current_cache.Generic (Solver)

let incremental ~(blacklist : string list) ~(opam : Git.Commit.t Current.t)
    (packages : Track.t list Current.t) : t Current.t =
  let open Current.Syntax in
  Current.component "incremental solver"
  |> let> opam = opam and> packages = packages in
     SolverCache.run No_context { packages; blacklist; platform = Platform.platform_amd64 } opam
