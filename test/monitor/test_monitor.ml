let step1 = Current_git.clone 
  ~schedule:(Current_cache.Schedule.v ())
  ~gref:"main"
  "https://github.com/ocurrent/ocaml-docs-ci.git"

let step2 = Current_git.clone 
  ~schedule:(Current_cache.Schedule.v ())
  "https://google.com/"

let step3: unit Current.t = Current.fail "oh no"

let running = 
  Current_docker.Default.build ~level:Current.Level.Dangerous
  ~pull:false
  `No_context

let fakepkg ~blessing name =
  let open Docs_ci_lib in
  let root = OpamPackage.of_string name in
  let pkg = Package.make 
    ~blacklist:[]
    ~commit:"0"
    ~root
    []
  in
  let blessing =
    let set = 
      Package.Blessing.Set.v
        ~counts:(Package.Map.singleton pkg 0)
        [pkg]
    in
    OpamPackage.Map.add
      root
      (Current.return set)
      blessing
  in
  pkg, blessing


let pipeline monitor =
  let open Docs_ci_lib in
  let blessing = OpamPackage.Map.empty in
  let pkg, blessing = fakepkg ~blessing "docs-ci.1.0.0" in
  let pkg2, blessing = fakepkg ~blessing "ocurrent.1.1.0" in
  let pkg3, blessing = fakepkg ~blessing "ocluster.0.7.0" in

  let values =
    [pkg, Monitor.(
      Seq [
        ("step1", Item step1);
        ("step2", Item step2);
        ("and-pattern", And [
          ("sub-step3", Item step1);
          ("sub-step4", Item step3);
        ])
      ]
    );
    pkg2, Monitor.(
      Seq [
        ("fake running step", Item running);
      ]
    );
    pkg3, Monitor.(
      Item step1
    )
    ]
    |> List.to_seq
    |> Package.Map.of_seq
  in
  Monitor.(register monitor blessing values);

  Current.all [
    step1 |> Current.map ignore;
    step2 |> Current.map ignore;
    step3 |> Current.map ignore;
    running |> Current.map ignore;
  ]

let () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_level (Some Debug);
  Logs.set_reporter (Logs_fmt.reporter ())

let main mode =
  let monitor = Docs_ci_lib.Monitor.make () in
  let engine =
    Current.Engine.create 
      ~config:(Current.Config.v ~confirm:Current.Level.Average ()) 
      (fun () -> pipeline monitor)
  in
  let has_role = Current_web.Site.allow_all in
  let site =
    let routes =
      Current_web.routes engine
      @ Docs_ci_lib.Monitor.routes monitor engine
    in
    Current_web.Site.(v ~has_role) ~name:"test_monitor" routes
  in
  ignore @@
  Lwt_main.run
    (Lwt.choose
       [
         Current.Engine.thread engine;
         (* The main thread evaluating the pipeline. *)
         Current_web.run ~mode site (* Optional: provides a web UI *);
       ])

(* Command-line parsing *)

open Cmdliner

let cmd =
  let info = Cmd.info "test_monitor" in
  Cmd.v info 
    Term.( const main $ Current_web.cmdliner)

let () = exit @@ Cmd.eval cmd
       