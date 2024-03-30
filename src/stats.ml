open Lwt.Infix
open Current.Syntax
open Docs_ci_lib
module Git = Current_git

let program_name = "stats"
let job_deps_sha job = job.Jobs.install |> Package.universes_hash

let ratio ~all ~part =
  if all = 0 then 0.0
  else
    let all = float_of_int all in
    let part = float_of_int part in
    part /. all *. 100.

let schedule_jobs ~targets all_packages_jobs =
  (* Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  Jobs.schedule ~targets all_packages_jobs |> List.sort Jobs.compare

let summary ~job ~all_packages_jobs_success ~all_opam_packages_failures
    ~all_opam_packages_success ~reduced_jobs ~schedule_jobs ~kind =
  Current.Job.log job "%s> The solved jobs success    : %d" kind
    all_packages_jobs_success;

  Current.Job.log job "%s> The opam packages success  : %d" kind
    all_opam_packages_success;

  Current.Job.log job "%s> The opam packages failures : %d" kind
    all_opam_packages_failures;

  Current.Job.log job "%s> Scheduled jobs             : %d" kind schedule_jobs;

  Current.Job.log job "%s> Coverage                   : %d/%d (%.2f ratio)" kind
    all_opam_packages_success
    (all_opam_packages_success + all_opam_packages_failures)
    (ratio
       ~all:(all_opam_packages_success + all_opam_packages_failures)
       ~part:all_opam_packages_success);

  Current.Job.log job "%s> Reduced jobs (by deps hashes) : %d" kind reduced_jobs

let solve_summary job solve kind =
  let all_failures = solve |> Solver.failures in
  let all_packages_jobs = solve |> Solver.keys |> List.rev_map Solver.get in
  let all_packages =
    all_packages_jobs |> List.rev_map Package.all_deps |> List.flatten
  in
  let schedule_jobs =
    schedule_jobs
      ~targets:(all_packages |> Package.Set.of_list)
      all_packages_jobs
  in
  let all_opam_packages =
    all_packages
    |> List.rev_map (fun pkg -> Package.opam pkg)
    |> OpamPackage.Set.of_list
  in
  let all_opam_packages_failures =
    List.rev_map fst all_failures
    |> List.flatten
    |> List.filter (fun opam ->
           not (OpamPackage.Set.mem opam all_opam_packages))
  in
  let reduced_jobs =
    let uniq_deps = Hashtbl.create 100 in
    List.iter
      (fun job -> Hashtbl.replace uniq_deps (job_deps_sha job) job)
      schedule_jobs;
    Hashtbl.to_seq_values uniq_deps |> List.of_seq |> List.sort Jobs.compare
  in
  summary ~job
    ~all_packages_jobs_success:(List.length all_packages_jobs)
    ~all_opam_packages_success:(OpamPackage.Set.cardinal all_opam_packages)
    ~all_opam_packages_failures:
      (OpamPackage.Set.cardinal
      @@ OpamPackage.Set.of_list all_opam_packages_failures)
    ~reduced_jobs:(List.length reduced_jobs)
    ~schedule_jobs:(List.length schedule_jobs)
    ~kind

module Stats = struct
  type t = { solve_group : Solver.t; solve_ungroup : Solver.t }

  let id = "show-cmd"

  module Key = Current.String
  module Value = Current.Unit

  let build solve job key =
    Current.Job.start job ~level:Current.Level.Harmless >>= fun () ->
    Current.Job.log job "opam-repository commit : %s" key;
    let { solve_ungroup; solve_group } = solve in

    (* Ungroup packages (current implementation) *)
    Current.Job.log job "";
    Current.Job.log job "Ungrouped packages (default)";
    solve_summary job solve_ungroup "default";

    (* Gropued packages by repo *)
    Current.Job.log job "";
    Current.Job.log job "Grouped packages by repo";
    solve_summary job solve_group "grouped";

    let all_packages_ungrouped_solved =
      solve_ungroup
      |> Solver.keys
      |> List.rev_map Solver.get
      |> List.rev_map (fun pkg -> (Package.opam pkg, pkg))
      |> OpamPackage.Map.of_list
    in
    let all_failures = solve_group |> Solver.failures in
    let all_packages_jobs =
      solve_group |> Solver.keys |> List.rev_map Solver.get
    in
    let all_packages =
      all_packages_jobs |> List.rev_map Package.all_deps |> List.flatten
    in
    let all_opam_packages =
      all_packages
      |> List.rev_map (fun pkg -> Package.opam pkg)
      |> OpamPackage.Set.of_list
    in
    let all_opam_packages_failures =
      List.rev_map fst all_failures
      |> List.flatten
      |> List.filter (fun opam ->
             not (OpamPackage.Set.mem opam all_opam_packages))
    in
    let all_packages_jobs_back =
      all_opam_packages_failures
      |> List.filter_map (fun opam_pkg ->
             OpamPackage.Map.find_opt opam_pkg all_packages_ungrouped_solved)
    in
    let all_opam_packages_back =
      all_packages_jobs_back
      |> List.rev_map Package.all_deps
      |> List.flatten
      |> List.rev_map Package.opam
      |> OpamPackage.Set.of_list
    in
    let all_opam_packages_failures =
      List.filter
        (fun opam_pkg ->
          not (OpamPackage.Set.mem opam_pkg all_opam_packages_back))
        all_opam_packages_failures
    in
    let all_opam_packages =
      OpamPackage.Set.union all_opam_packages all_opam_packages_back
    in
    let all_packages_jobs = all_packages_jobs @ all_packages_jobs_back in
    let schedule_jobs =
      schedule_jobs
        ~targets:(all_packages |> Package.Set.of_list)
        all_packages_jobs
    in
    let reduced_jobs =
      let uniq_deps = Hashtbl.create 100 in
      List.iter
        (fun job -> Hashtbl.replace uniq_deps (job_deps_sha job) job)
        schedule_jobs;
      Hashtbl.to_seq_values uniq_deps |> List.of_seq |> List.sort Jobs.compare
    in
    Current.Job.log job "";
    Current.Job.log job
      "Grouped packages by repo (take the failures that are solved individualy)";
    summary ~job
      ~all_packages_jobs_success:(List.length all_packages_jobs)
      ~all_opam_packages_success:(OpamPackage.Set.cardinal all_opam_packages)
      ~all_opam_packages_failures:
        (OpamPackage.Set.cardinal
        @@ OpamPackage.Set.of_list all_opam_packages_failures)
      ~reduced_jobs:(List.length reduced_jobs)
      ~schedule_jobs:(List.length schedule_jobs)
      ~kind:"group-fixed";

    Lwt.return @@ Ok ()

  let pp = Key.pp
  let auto_cancel = true
end

module Stats_cache = Current_cache.Make (Stats)

let stats solve hash kind =
  Current.component "stats (%s)" kind
  |>
  let> key = hash and> solve in
  Stats_cache.get solve key

let () = Prometheus_unix.Logging.init ()

let pipeline ~repo ~limit () =
  let opam = Git.Local.head_commit repo in
  let tracked = Track.v ~limit ~filter:[] opam in
  let tracked_group = Track.v ~group:true ~limit ~filter:[] opam in
  let solver_result_c =
    Solver.incremental ~nb_jobs:6 ~blacklist:[] ~opam tracked
  in
  let solver_result_c_group =
    Current.bind
      (fun _ ->
        Solver.incremental ~group:true ~nb_jobs:6 ~blacklist:[] ~opam
          tracked_group)
      solver_result_c
  in
  let hash opam = Current.map (fun opam -> Git.Commit.hash opam) opam in
  let* solve_ungroup = solver_result_c in
  let* solve_group = solver_result_c_group in
  let solve = Current.return { Stats.solve_group; solve_ungroup } in
  Current.all [ stats solve (hash opam) "Summary" ]

let main () mode limit repo =
  (* Here, we set the config to request a confirmation above job with [Average] value. *)
  let config = Current.Config.v ~confirm:Current.Level.Average () in
  Lwt_main.run
    (let repo = Git.Local.v (Fpath.v repo) in
     let engine = Current.Engine.create ~config (pipeline ~repo ~limit) in
     let site =
       Current_web.Site.(v ~has_role:allow_all)
         ~name:program_name
         (Current_web.routes engine)
     in
     Lwt.choose [ Current.Engine.thread engine; Current_web.run ~mode site ])

open Cmdliner

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Logging.init ();
  Memtrace.trace_if_requested ~context:"stats" ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Logs_cli.level ~docs ())

let repo =
  Arg.required
  @@ Arg.pos 0 Arg.(some dir) None
  @@ Arg.info ~doc:"The directory contains the .git subdirectory." ~docv:"DIR"
       []

let limit =
  Arg.value
  @@ Arg.opt Arg.(some int) (Some 2000)
  @@ Arg.info ~doc:"The limit of versions of each package" [ "versions-limit" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "Universes pipeline stats" in
  let info = Cmd.info program_name ~doc ~version in
  Cmd.v info
    Term.(
      term_result (const main $ setup_log $ Current_web.cmdliner $ limit $ repo))
(* $ Docs_ci_lib.Config.cmdliner) *)

let () = exit @@ Cmd.eval cmd
