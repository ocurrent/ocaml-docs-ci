(* docker manifest inspect ocaml/opam:ubuntu-ocaml-4.13

   amd64: sha256:c2278d6ff88b3fc701a04f57231c95f78f0cc38dd2541f90f99e2cb15e96a0aa
*)

module Platform : sig
  val v : packages:Package.t list -> Ocaml_version.t option
  val to_string : Ocaml_version.t -> string
end = struct
  let v ~packages =
    let ( let* ) = Option.bind in
    let ocaml_version name =
      packages
      |> List.find_opt (fun pkg -> pkg |> Package.opam |> OpamPackage.name_to_string = name)
      |> Option.map (fun pkg -> pkg |> Package.opam |> OpamPackage.version_to_string)
    in
    let is_base =
      List.exists
        (fun p -> Package.opam p |> OpamPackage.name_to_string = "ocaml-base-compiler")
        packages
    in
    let* version =
      if is_base then ocaml_version "ocaml-base-compiler" else ocaml_version "ocaml-variants"
    in
    Ocaml_version.of_string version |> Result.to_option

  let to_string v = Ocaml_version.to_string v
end

let tag ocaml_version =
  let minor =
    if Ocaml_version.major ocaml_version >= 5 then
      Fmt.str "%d" (Ocaml_version.minor ocaml_version)
    else
      Fmt.str "%02d" (Ocaml_version.minor ocaml_version)
  in
  Fmt.str "debian-11-ocaml-%d.%s%s"
    (Ocaml_version.major ocaml_version)
    minor
    (match Ocaml_version.extra ocaml_version with
    | None -> ""
    | Some x -> "-" ^ x |> String.map (function '+' -> '-' | x -> x))
module PeekerBody  = struct
  type t = unit

  let id = "docker-peek"

  module Key = struct
    type t = Ocaml_version.t
    let digest x = "v2"^Ocaml_version.to_string x
  end

  module Value = struct
    type t = string
    let marshal x = x
    let unmarshal x = x
  end



  let conv_error = function
    | Ok x -> Ok x
    | Error (`Malformed_json s) -> Error (`Msg ("Malformed json: "^s))
    | Error `No_corresponding_arch_found -> Error (`Msg "No corresponding arch found")
    | Error `No_corresponding_os_found -> Error (`Msg "No corresponding OS found")

  let build () job key =
    let open Lwt.Syntax in
    let* () = Current.Job.start ~level:Current.Level.Mostly_harmless job in
    Current.Job.log job "tag: %s" (tag key);
    let+ res = Docker_hub.fetch_manifests ~repo:"ocaml/opam" ~tag:(Some (tag key)) in
    match res with
    | Ok manifests ->
      Result.map (fun r ->
        let tag = "ocaml/opam@"^r in
        Current.Job.log job "result: %s" tag;
        tag) (Docker_hub.digest ~os:"linux" ~arch:"amd64" manifests |> conv_error)
    | Error (`Msg _) as e -> e
    | Error (`Api_error (_response, _opt)) -> Error (`Msg "Api_error")
    | Error (`Malformed_json str) -> Error (`Msg ("Malformed_json" ^ str))

  let pp = Ocaml_version.pp

  let auto_cancel = true
end

module Peeker = Current_cache.Make(PeekerBody)
module Image : sig
  val peek : Ocaml_version.t -> string Current.t
end = struct
  let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) ()
  let real_peek ocaml_version =
    (* let* _ = Current_docker.Default.pull ~schedule:weekly ~arch tag in *)
    Peeker.get ~schedule:weekly () ocaml_version

  let peek ocaml_version =
    let tag = tag ocaml_version in
    Current.primitive ~info:(Current.component "Docker image peek %s" tag)
      (fun () -> real_peek ocaml_version) (Current.return ())
end

let cache_hint package =
  let packages = Package.all_deps package in
  Platform.v ~packages |> Option.value ~default:Ocaml_version.Releases.latest |> Platform.to_string

(** Select base image to use *)
let get_base_image packages =
  let open Current.Syntax in
  let version = Platform.v ~packages |> Option.value ~default:Ocaml_version.Releases.latest in
  let+ tag = Image.peek version in
  Spec.make tag

let default_base_image =
  let open Current.Syntax in
  let version = Ocaml_version.Releases.latest in
  let+ tag = Image.peek version in
  Spec.make tag

let spec_of_job job =
  let install = job.Jobs.install in
  let all_deps = Package.all_deps install in
  try get_base_image all_deps
  with e ->
    Format.eprintf "Error with job: %a" (Fmt.list Package.pp) all_deps;
    raise e

let network = [ "host" ]
let docs_cache_folder = "/home/opam/docs-cache/"
let cache = [ Obuilder_spec.Cache.v ~target:docs_cache_folder "ci-docs" ]

(** Obuilder operation to locally pull the selected folders. The [digests] option
is used to invalidate the operation if the expected value changes. *)
let rsync_pull ~ssh ?(digest = "") folders =
  let sources =
    List.map
      (fun folder ->
        Fmt.str "%s:%s/./%a" (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh) Fpath.pp folder)
      folders
    |> String.concat " "
  in
  let cache_sources =
    List.map (Fmt.str "%s./%a" docs_cache_folder Fpath.pp) folders |> String.concat " "
  in
  match folders with
  | [] -> Obuilder_spec.comment "no sources to pull"
  | _ ->
      Obuilder_spec.run ~secrets:Config.Ssh.secrets ~cache ~network
        "rsync --delete -avzR %s %s  && rsync -aR %s ./ && echo 'pulled: %s'" sources
        docs_cache_folder cache_sources digest

module LatchedBuilder (B : Current_cache.S.BUILDER) = struct
  module Adaptor = struct
    type t = B.t

    let id = B.id

    module Key = Current.String
    module Value = B.Key
    module Outcome = B.Value

    let run op job _ key = B.build op job key
    let pp f (_, key) = B.pp f key
    let auto_cancel = B.auto_cancel
    let latched = true
  end

  include Current_cache.Generic (Adaptor)

  let get ~opkey ?schedule ctx key = run ?schedule ctx opkey key
end

let profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some "docker" -> `Docker
  | Some x -> Fmt.failwith "Unknown $PROFILE setting %S" x

let to_obuilder_job build_spec = Fmt.to_to_string Obuilder_spec.pp (build_spec |> Spec.finish)

let to_docker_job build_spec =
  let spec_str =
    Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:true ~os:`Unix (build_spec |> Spec.finish)
  in
  `Contents spec_str

let to_ocluster_submission spec =
  match profile with
  | `Production | `Dev -> to_obuilder_job spec |> Cluster_api.Submission.obuilder_build
  | `Docker -> to_docker_job spec |> Cluster_api.Submission.docker_build

let with_error_check fn:('a -> string -> 'a) =
  fun x s ->
    match x with
    | Ok y -> Ok ((fn y) s)
    | Error _ -> Error ()

let fold_logs build_job fn =
  (* TODO: what if we encounter an infinitely long line ? *)
  let open Lwt.Syntax in
  let rec aux start next_lines acc =
    match next_lines with
    | ([] | [ _ ]) as e -> (
        let prev_line = match e with [] -> "" | e :: _ -> e in
        let* logs = Cluster_api.Job.log build_job start in
        match (logs, prev_line) with
        | Error (`Capnp e), _ -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
        | Ok ("", _), "" -> Lwt_result.return acc
        | Ok ("", _), last_line -> aux start [ last_line; "" ] acc
        | Ok (data, next), prev_line ->
            let lines = String.split_on_char '\n' data in
            let fst = List.hd lines in
            let rest = List.tl lines in
            aux next ((prev_line ^ fst) :: rest) acc)
    | line :: next -> aux start next (fn acc line)
  in
  aux 0L []

let tar_cmd folder =
  let f = Fpath.to_string folder in
  Fmt.str
    "shopt -s nullglob && ((tar -cvf %s.tar %s/*  && rm -R %s/* && mv %s.tar %s/content.tar) || \
     (echo 'Empty directory'))"
    f f f f f

module Cmd = struct
  let tar = tar_cmd

  let list =
    let open Fmt in
    to_to_string (list ~sep:(const string " && ") (fun f -> pf f "(%s)"))
end

module Retry : sig

    (* extends [fn] to check if the line has an error message *)
  val retry_loop:
    ?sleep_duration:(int -> float) ->
    ?log_string: string ->
    ?number_of_attempts:int ->
    ?max_number_of_attempts:int ->
    (unit -> (('a * 'c list), 'e) Lwt_result.t) ->
    ('a, 'e) Lwt_result.t

end = struct

  open Lwt.Infix
  let ( let* ) = Lwt.bind

  let base_sleep_time = 30

  let sleep_duration n' =
    (* backoff is based on n *. 30. *. (Float.pow 1.5 n)
      This gives the sequence 0s -> 45s -> 135s -> 300s -> 600s -> 1100s
    *)
    let n = Int.to_float n' in
    let randomised_sleep_time = base_sleep_time + (Random.int 20) in
    let backoff = (n *. Int.to_float base_sleep_time *. Float.pow 1.5 n) in
    Int.to_float randomised_sleep_time +. backoff

  let rec retry_loop
    ?(sleep_duration=sleep_duration)
    ?(log_string="")
    ?(number_of_attempts=0)
    ?(max_number_of_attempts=2)
    fn_returning_results_and_retriable_errors =

    let log_line = Printf.sprintf "RETRYING: %s Number of retries: %d" (log_string) number_of_attempts in
    let* x = fn_returning_results_and_retriable_errors () in
    match x with
    | Error e ->
      if number_of_attempts <= max_number_of_attempts then
        Lwt_unix.sleep (sleep_duration @@ number_of_attempts) >>= fun () ->
          Log.info (fun f -> f "%s" log_line);
          retry_loop ~sleep_duration ~log_string ~number_of_attempts:(number_of_attempts + 1) ~max_number_of_attempts fn_returning_results_and_retriable_errors
      else
        Lwt.return_error e
    | Ok (results, retriable_errors) ->
      if retriable_errors != [] && number_of_attempts <= max_number_of_attempts then
        Lwt_unix.sleep (sleep_duration @@ number_of_attempts) >>= fun () ->
          Log.info (fun f -> f "%s" log_line);
          retry_loop ~sleep_duration ~log_string ~number_of_attempts:(number_of_attempts + 1) ~max_number_of_attempts fn_returning_results_and_retriable_errors
      else
        Lwt.return_ok results
end
