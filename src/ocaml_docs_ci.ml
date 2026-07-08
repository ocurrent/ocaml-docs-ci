module Profile = Day11_batch.Profile

let setup_log default_level =
  Prometheus_unix.Logging.init ?default_level ();
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Memtrace.trace_if_requested ~context:"ocaml-docs-ci" ();
  (* Eio strips OCaml backtraces by the time uncaught exns surface in
     the supervisor. Hook [set_uncaught_exception_handler] so the
     runtime prints the {e raw} backtrace from the actual throw site
     before Eio re-raises. *)
  Printexc.record_backtrace true;
  Printexc.set_uncaught_exception_handler (fun exn raw_bt ->
    Fmt.epr "FATAL: %s@.%s@."
      (Printexc.to_string exn)
      (Printexc.raw_backtrace_to_string raw_bt))

let program_name = "ocaml-docs-ci"

let has_role user = function
  | `Viewer | `Monitor -> true
  | _ -> (
      match Option.map Current_web.User.id user with
      | Some
          ( "github:talex5" | "github:avsm" | "github:kit-ty-kate"
          | "github:samoht" | "github:tmcgilchrist" | "github:dra27"
          | "github:jonludlam" | "github:TheLortex" | "github:sabine"
          | "github:mtelvers" | "github:shonfeder" ) ->
          true
      | _ -> false)

(* Load profile JSON only. The heavy [Profile_ctx.t] construction
   (git_packages / hash_cache) is deferred to a per-tick OCurrent Op
   in [Day11_profile_ctx_loader], so changes to any opam repository
   (remote or local) flow through the pipeline and invalidate the
   solver cache. *)
let load_profiles ~profile_dir names : Day11_batch.Profile.t list =
  List.map (fun name ->
    match Profile.load ~dir:profile_dir ~name with
    | Error (`Msg e) ->
      Fmt.epr "error loading profile %s: %s@." name e;
      exit 2
    | Ok profile ->
      if profile.opam_repositories = [] then begin
        Fmt.epr "profile %s has empty opam_repositories@." name;
        exit 2
      end;
      profile
  ) names

(* Root filesystem usage percent from [df -P /] (POSIX one-line format:
   FS 1024-blocks Used Avail Capacity% Mounted-on). Returns -1 if [df]
   fails or the line can't be parsed, so the caller leaves the gauge
   untouched rather than recording a bogus value. *)
let df_root_percent () =
  try
    let ic = Unix.open_process_in "df -P /" in
    Fun.protect ~finally:(fun () -> ignore (Unix.close_process_in ic))
      (fun () ->
         let _header = input_line ic in
         let row = input_line ic in
         let cols =
           String.map (fun c -> if c = '\t' then ' ' else c) row
           |> String.split_on_char ' '
           |> List.filter (fun s -> s <> "")
         in
         match cols with
         | _fs :: _blocks :: _used :: _avail :: cap :: _ ->
           (try Scanf.sscanf cap "%d%%" Fun.id with _ -> -1)
         | _ -> -1)
  with _ -> -1

let main () current_config github_auth mode profiles_arg profile_dir_arg
    cache_dir_arg remotes_arg pin_overlays_arg cores_per_build overcommit
    config : unit =
  (* The epoch-promote node runs at [Current.Level.Dangerous]. By default
     OCurrent's [--confirm] is unset (nothing is gated), so it would fire
     immediately — defeating the manual-promote design. Default the
     confirmation threshold to [Dangerous] so *only* promotion waits for a
     click in the web UI; all the build/prep ops run at [Average] or below
     and so still run unattended. An explicit [--confirm] (or the web UI
     slider) overrides this. *)
  (match Current.Config.get_confirm current_config with
   | Some _ -> ()
   | None ->
     Current.Config.set_confirm current_config (Some Current.Level.Dangerous));
  let profile_dir =
    Fpath.v (match profile_dir_arg with
      | Some d -> d
      | None ->
        Filename.concat (Sys.getenv "HOME") ".day11/profiles")
  in
  let cache_dir =
    Fpath.v (match cache_dir_arg with
      | Some d -> d
      | None ->
        Filename.concat (Sys.getenv "HOME") ".day11/cache")
  in
  ignore (Bos.OS.Dir.create ~path:true cache_dir);
  Docs_ci_lib.Startup_diagnostics.run ~profile_dir ~cache_dir;
  (* A relative [--remote] / [--github-pin-overlay] PATH is resolved
     against the .day11 root — the same base [Profile.load] uses for a
     profile's [opam_repositories] ([profile_dir]'s parent). Resolving
     both the same way is what lets a relative spec path line up with
     the matching relative entry in a profile (the pipeline keys live
     repo-polling on the resolved path string). *)
  let day11_dir = Fpath.parent profile_dir in
  let resolve_repo_path p =
    Fpath.v (Profile.resolve_repo ~day11_dir (Fpath.to_string p)) in
  let remote_specs =
    List.map (fun arg ->
      match Docs_ci_lib.Remote_opam_repo.spec_of_arg arg with
      | Ok s -> { s with path = resolve_repo_path s.path }
      | Error (`Msg e) ->
        Fmt.epr "error: %s@." e;
        exit 2
    ) remotes_arg
  in
  if remote_specs <> [] then
    Logs.app (fun f -> f "Maintaining %d remote opam repo%s"
      (List.length remote_specs)
      (if List.length remote_specs = 1 then "" else "s"));
  let pin_overlay_specs =
    List.map (fun arg ->
      match Docs_ci_lib.Github_pin_overlay.spec_of_arg arg with
      | Ok s -> { s with path = resolve_repo_path s.path }
      | Error (`Msg e) ->
        Fmt.epr "error: %s@." e;
        exit 2
    ) pin_overlays_arg
  in
  if pin_overlay_specs <> [] then
    Logs.app (fun f -> f "Maintaining %d github pin overlay%s"
      (List.length pin_overlay_specs)
      (if List.length pin_overlay_specs = 1 then "" else "s"));
  let names =
    match profiles_arg with
    | [] ->
      (* Default: every profile in [profile_dir]. *)
      Profile.list ~dir:profile_dir
    | ns -> ns
  in
  if names = [] then begin
    Fmt.epr "no profiles in %a and none named on the command line@."
      Fpath.pp profile_dir;
    exit 2
  end;
  Logs.app (fun f -> f "Loading %d profile%s: %s"
    (List.length names) (if List.length names = 1 then "" else "s")
    (String.concat ", " names));
  let profiles = load_profiles ~profile_dir names in
  (* Optional NUMA-aware cpu slot pool. When configured, every
     container launch acquires a slot with a pinned cpuset +
     NUMA-local memory node, capping nested build parallelism
     via cgroup v2. *)
  let cpu_slots = match cores_per_build with
    | None | Some 0 -> None
    | Some n ->
      let pool =
        Day11_runner.Cpu_slots.auto ~cores_per_build:n ~overcommit () in
      Logs.app (fun f -> f "CPU pool: %s"
        (Day11_runner.Cpu_slots.describe pool));
      Some pool
  in
  ignore @@ Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:(Eio.Stdenv.clock env) @@ fun _token ->
  let eio_env = (env :> Eio_unix.Stdenv.base) in
  (* Map from a profile's [opam_repositories] entry (a local path) to
     a live [Current_git.Commit.t Current.t] driven by
     [Remote_opam_repo.maintain_commit]. The pipeline reads HEAD
     directly from this map — no inotify, no filesystem watcher in
     between. A profile entry whose path isn't backed by a [--remote]
     spec falls back to a one-shot read of HEAD at startup. *)
  let remote_schedule =
    Current_cache.Schedule.v ~valid_for:(Duration.of_hour 1) () in
  let remote_commits :
    (string, Current_git.Commit.t Current.t) Hashtbl.t =
    Hashtbl.create (List.length remote_specs) in
  List.iter (fun (s : Docs_ci_lib.Remote_opam_repo.spec) ->
    let commit = Docs_ci_lib.Remote_opam_repo.maintain_commit
      ~schedule:remote_schedule ~url:s.url ~path:s.path in
    Hashtbl.replace remote_commits (Fpath.to_string s.path) commit
  ) remote_specs;
  (* Github-pin overlays: each spec generates an opam-repo overlay
     under [<path>/repo/] from upstream HEAD on the same hourly
     schedule. Profiles reference the overlay path the same way they
     reference [--remote] paths — and since the overlay is itself a
     git repo whose SHA changes when (and only when) upstream moves,
     [Profile_ctx_loader] picks up the change without any further
     plumbing. *)
  List.iter (fun (s : Docs_ci_lib.Github_pin_overlay.spec) ->
    let commit = Docs_ci_lib.Github_pin_overlay.maintain_commit
      ?branch:s.branch ~schedule:remote_schedule ~url:s.url ~path:s.path () in
    let overlay_path =
      Fpath.to_string (Fpath.(s.path / "repo")) in
    Hashtbl.replace remote_commits overlay_path commit
  ) pin_overlay_specs;
  let engine =
    Current.Engine.create ~config:current_config (fun () ->
      Docs_ci_pipelines.Docs.v ~config
        ~eio_env ~cache_dir ~profiles ~remote_commits ?cpu_slots ()
      |> Current.ignore_value)
  in
  (* Self-heal cache/disk divergence at startup: a [day11-node] success
     is keyed on layer hash alone and doesn't track whether the layer
     dir still exists, so a layer removed out-of-band (layer GC by
     last-used, an os_dir migration, manual cleanup) leaves a stale
     success that makes OCurrent skip the rebuild forever — the node
     shows as permanently "pending". Invalidate those so the first
     evaluation re-dispatches them. See [Day11_prep.reconcile_cache]. *)
  (try
     let n = Docs_ci_lib.Day11_prep.reconcile_cache () in
     if n > 0 then
       Logs.app (fun f ->
         f "cache reconcile: invalidated %d layer build(s) whose layer \
            dir was missing" n)
   with e ->
     Logs.warn (fun f ->
       f "cache reconcile skipped: %s" (Printexc.to_string e)));
  let has_role =
    if github_auth = None then Current_web.Site.allow_all else has_role
  in
  let secure_cookies = github_auth <> None in
  let authn = Option.map Current_github.Auth.make_login_uri github_auth in
  let site =
    let dashboard_routes =
      Docs_ci_web.Web_routes.routes
        ~ctx:{ profile_dir; cache_dir } in
    let routes =
      Routes.[
        (s "login" /? nil) @--> Current_github.Auth.login github_auth;
        (* Custom index with an about blurb; first match wins, so these
           shadow the stock Current_web index below. *)
        (nil @--> (Docs_ci_web.Index_page.r ~engine :> Current_web.Resource.t));
        (s "index.html" /? nil
         @--> (Docs_ci_web.Index_page.r ~engine :> Current_web.Resource.t));
        (* Prometheus scrape target (text exposition format). Served on
           the dashboard port; unauthenticated, like a normal target. *)
        (s "metrics" /? nil
         @--> (Docs_ci_web.Metrics_page.r :> Current_web.Resource.t));
      ]
      @ dashboard_routes
      @ Current_web.routes engine
    in
    Current_web.Site.(v ?authn ~has_role ~secure_cookies)
      ~name:program_name routes
  in
  (* Host disk metrics, sampled periodically. The layer-metadata total
     reads one [layer.json] per layer (300k+ at scale, ~tens of seconds),
     so it runs in a systhread — via [Lwt_eio.run_eio] +
     [Eio_unix.run_in_systhread] — to keep the engine and web responsive.
     It must NOT spawn a domain: once any domain has been spawned OCaml 5
     permanently forbids [Unix.fork], which the fork-helper/runc build
     path relies on. A systhread carries no such restriction. [df] is
     cheap enough to run inline. First sample fires at startup, then every
     [disk_sample_period] seconds. *)
  let disk_sample_period = 600.0 in
  let disk_metrics_thread =
    let rec loop () =
      let root_percent = df_root_percent () in
      Lwt.bind
        (Lwt_eio.run_eio (fun () ->
           Eio_unix.run_in_systhread (fun () ->
             Day11_lib.Disk_usage.layer_meta_total ~cache_dir)))
        (fun layer_bytes ->
           Docs_ci_lib.Metrics.set_disk ~root_percent ~layer_bytes;
           Lwt.bind (Lwt_unix.sleep disk_sample_period) loop)
    in
    loop ()
  in
  Lwt_eio.Promise.await_lwt (Lwt.choose
    [
      Current.Engine.thread engine;
      Current_web.run ~mode site;
      disk_metrics_thread;
    ])

open Cmdliner

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(const setup_log $ Logs_cli.level ~docs ())

let profiles_arg =
  Arg.value
  @@ Arg.opt Arg.(list string) []
  @@ Arg.info ~doc:"Comma-separated list of day11 profile names to run. \
                    If unset, every profile in --profile-dir is used."
       ~docv:"PROFILES" [ "profiles" ]

let profile_dir_arg =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"Directory containing day11 profile JSON files. \
                    Defaults to ~/.day11/profiles."
       ~docv:"DIR" [ "profile-dir" ]

let cache_dir_arg =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"day11 cache root. Defaults to ~/.day11/cache."
       ~docv:"DIR" [ "cache-dir" ]

let remotes_arg =
  Arg.value
  @@ Arg.opt_all Arg.string []
  @@ Arg.info
       ~doc:"Mirror a remote opam-repository into a local path. \
             Repeatable. Format: $(b,URL=PATH). ocaml-docs-ci clones \
             $(b,URL) into $(b,PATH) at startup and fetches it \
             hourly; local commits are preserved (fast-forward-only \
             merge, fails if the working tree has diverged). Day11 \
             profiles reference $(b,PATH) as a regular local repo. \
             A relative $(b,PATH) is resolved against the .day11 root \
             (--profile-dir's parent), the same as a profile's \
             $(i,opam_repositories) entries — so a relative spec path \
             lines up with the matching relative entry in a profile."
       ~docv:"URL=PATH" [ "remote" ]

let pin_overlays_arg =
  Arg.value
  @@ Arg.opt_all Arg.string []
  @@ Arg.info
       ~doc:"Track a github URL and republish its $(b,*.opam) files \
             as a synthetic opam-repo overlay. Repeatable. Format: \
             $(b,URL[#BRANCH]=PATH); an optional $(b,#BRANCH) suffix \
             tracks a non-default branch (e.g. a fork's feature branch). \
             On the same hourly schedule as $(b,--remote), \
             ocaml-docs-ci clones $(b,URL) into $(b,PATH/upstream/), \
             rewrites each $(b,*.opam) with $(i,version:) set to \
             $(i,<latest-tag>+<branch>.<commit-epoch>.<sha7>) and \
             $(i,src:) pointing at the pinned commit, and commits the \
             result to $(b,PATH/repo/) (its own git repo). Profiles \
             reference $(b,PATH/repo) as a regular local repo. As with \
             $(b,--remote), a relative $(b,PATH) is resolved against \
             the .day11 root (--profile-dir's parent)."
       ~docv:"URL[#BRANCH]=PATH" [ "github-pin-overlay" ]

let cores_per_build_arg =
  Arg.value
  @@ Arg.opt Arg.(some int) None
  @@ Arg.info
       ~doc:"Cores per container. Enables cgroup cpuset pinning and \
             NUMA-local memory allocation (when the host has 2+ NUMA \
             nodes). Host CPUs are split into slots of this size; \
             each container sees exactly N cpus via [nproc]. 0 / \
             unset disables pinning."
       ~docv:"N" [ "cores-per-build" ]

let overcommit_arg =
  Arg.value
  @@ Arg.opt Arg.float 1.0
  @@ Arg.info
       ~doc:"Multiplier on the strict CPU-bounded slot count. 1.0 \
             (default) gives each build exclusive cpus; 1.5 shares \
             cpusets 50% of the time; 2.0 doubles every cpuset. Only \
             effective when --cores-per-build is set."
       ~docv:"FACTOR" [ "overcommit" ]

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "OCaml documentation CI pipeline" in
  let info = Cmd.info program_name ~doc ~version in
  Cmd.v info
    Term.(
      const main
      $ setup_log
      $ Current.Config.cmdliner
      $ Current_github.Auth.cmdliner
      $ Current_web.cmdliner
      $ profiles_arg
      $ profile_dir_arg
      $ cache_dir_arg
      $ remotes_arg
      $ pin_overlays_arg
      $ cores_per_build_arg
      $ overcommit_arg
      $ Docs_ci_lib.Config.cmdliner)

let () = exit @@ Cmd.eval cmd
