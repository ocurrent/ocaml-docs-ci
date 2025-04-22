open Docs_ci_lib

module CurrentMap (Map : OpamStd.MAP) = struct
  let map_seq (input : 'a Current.t Map.t) : 'a Map.t Current.t =
    Map.bindings input
    |> List.rev_map (fun (k, v) -> Current.map (fun x -> (k, x)) v)
    |> Current.list_seq
    |> Current.map Map.of_list
end

module OpamPackageNameCurrentMap = CurrentMap (OpamPackage.Name.Map)
module OpamPackageVersionCurrentMap = CurrentMap (OpamPackage.Version.Map)

let status = function
  | Error (`Msg msg) -> Monitor.Err msg
  | Error (`Active _) -> Active
  | Error `Blocked -> Blocked
  | Ok _ -> OK

let job_id (metadata : Current.Metadata.t option) : string option =
  match metadata with None -> None | Some metadata -> metadata.job_id

open Current.Syntax

let rec current_list_flatten x =
  match x with
  | [] -> Current.return []
  | [ x' ] -> x'
  | x' :: y ->
      let+ x' and+ y' = current_list_flatten y in
      x' @ y'

let rec summarise description arr = function
  | Monitor.Item current ->
      let state = Current.state current in
      let metadata = Current.Analysis.metadata current in
      let+ status = Current.map status state
      and+ job_id = Current.map job_id metadata in
      { Monitor.typ = description; job_id; status } :: arr
  | Seq items | And items | Or items ->
      let f name = Fmt.str "%s" name in
      let f_colon name = ":" ^ Fmt.str "%s" name in

      List.map
        (fun (name, item) ->
          summarise
            (if description = "" then description ^ f name
             else description ^ f_colon name)
            arr item)
        items
      |> current_list_flatten

let count_refs (all : Package.Set.t) =
  let prep_jobs : int ref Package.Map.t ref = ref Package.Map.empty in

  let rec get_prep_job package =
    try
      let ref = Package.Map.find package !prep_jobs in
      incr ref
    with Not_found ->
      let dependencies = Package.universe package |> Package.Universe.deps in
      let () = List.iter (fun p -> get_prep_job p) dependencies in
      prep_jobs := Package.Map.add package (ref 1) !prep_jobs
  in
  Package.Set.iter (fun x -> get_prep_job x) all;
  !prep_jobs

type 'a compile_job = { job : 'a Current.t; monitor : Monitor.pipeline_tree }

let compile ~generation ~config
    ~(blessed : Package.Blessing.Set.t Current.t OpamPackage.Map.t)
    (preps : Prep.t compile_job Package.Map.t) =
  let compilation_jobs : Compile.t compile_job option Package.Map.t ref =
    ref Package.Map.empty
  in
  let link_jobs : Compile.t compile_job option Package.Map.t ref =
    ref Package.Map.empty
  in

  let rec get_compilation_job link package =
    let name = package |> Package.opam |> OpamPackage.to_string in
    let extra_deps =
      Package.universe package |> Package.Universe.extra_link_deps
    in
    let job_cache = if link then link_jobs else compilation_jobs in
    try Package.Map.find package !job_cache
    with Not_found -> (
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun { job = prep; _ } ->
           let dependencies =
             Package.universe package |> Package.Universe.deps
           in
           if List.length extra_deps > 0 then
             Logs.info (fun m ->
                 m "Extra link deps for %s: %s (link: %b)" name
                   (extra_deps
                   |> List.map (fun x ->
                          x |> Package.opam |> OpamPackage.to_string)
                   |> String.concat ", ")
                   link);
           let compile_dependencies_names =
             List.filter_map
               (fun p ->
                 get_compilation_job false p
                 |> Option.map (fun { job; _ } -> (p, job)))
               dependencies
           in
           let link_dependencies_names =
             if link then
               List.filter_map
                 (fun p ->
                   get_compilation_job false p
                   |> Option.map (fun { job; _ } -> (p, job)))
                 extra_deps
             else []
           in
           let compile_dependencies =
             compile_dependencies_names |> List.map snd
           in
           let link_dependencies = link_dependencies_names |> List.map snd in

           let blessing =
             OpamPackage.Map.find (Package.opam package) blessed
             |> Current.map (fun b -> Package.Blessing.Set.get b package)
           in
           let deps, jobty =
             match (List.length extra_deps, link) with
             | 0, _ ->
                 (Current.list_seq compile_dependencies, Compile.CompileAndLink)
             | _, false ->
                 Logs.info (fun m -> m "Creating CompileOnly job");
                 (Current.list_seq compile_dependencies, Compile.CompileOnly)
             | _, true ->
                 Logs.info (fun m -> m "Creating LinkOnly job");
                 ( Current.list_seq (compile_dependencies @ link_dependencies),
                   Compile.LinkOnly )
           in
           let node =
             Compile.v ~generation ~config ~name ~blessing ~deps ~jobty prep
           in
           let monitor =
             Monitor.(Seq [ ("do-deps", Item prep); ("do-compile", Item node) ])
           in
           (jobty, { job = node; monitor })
      in
      match job with
      | None -> None
      | Some (CompileAndLink, job) ->
          compilation_jobs :=
            Package.Map.add package (Some job) !compilation_jobs;
          link_jobs := Package.Map.add package (Some job) !link_jobs;
          Some job
      | Some (CompileOnly, job) ->
          compilation_jobs :=
            Package.Map.add package (Some job) !compilation_jobs;
          Some job
      | Some (LinkOnly, job) ->
          link_jobs := Package.Map.add package (Some job) !link_jobs;
          Some job)
  in
  let get_compilation_node package _ = get_compilation_job true package in
  let compile_jobs =
    Package.Map.filter_map get_compilation_node preps |> Package.Map.bindings
  in
  compile_jobs

let prep ~config ~opamfiles (all : Package.Set.t) =
  let prep_jobs : Prep.t compile_job Package.Map.t ref =
    ref Package.Map.empty
  in

  let rec get_prep_job package =
    try Package.Map.find package !prep_jobs
    with Not_found ->
      let job =
        let dependencies = Package.universe package |> Package.Universe.deps in
        let dep_opams =
          List.map Package.opam dependencies |> OpamPackage.Set.of_list
        in
        let opams =
          OpamPackage.Set.add (Package.opam package) dep_opams
          |> OpamPackage.Set.to_list
        in
        let ocaml_version = Package.ocaml_version package in
        let all_opams = Prep.add_base ocaml_version opams in

        let prep_dependencies_names =
          List.map
            (fun p -> get_prep_job p |> fun { job; _ } -> (p, job))
            dependencies
        in
        let prep_dependencies = prep_dependencies_names |> List.map snd in
        let base_image = Misc.get_base_image package in
        let opamfiles =
          Current.map
            (fun x ->
              List.filter_map
                (fun p ->
                  try Some (p, OpamPackage.Map.find p x) with _ -> None)
                all_opams
              |> OpamPackage.Map.of_list)
            opamfiles
        in
        let node =
          Prep.v ~config
            ~deps:(Current.list_seq prep_dependencies)
            ~spec:base_image ~opamfiles ~prep:package
        in
        let monitor =
          Monitor.(
            Seq
              [
                ( "prep",
                  And
                    (("prep " ^ Package.id package, Item node)
                    :: List.map
                         (fun (pkg, compile) ->
                           ("prep dependency " ^ Package.id pkg, Item compile))
                         prep_dependencies_names) );
              ])
        in
        { job = node; monitor }
      in

      prep_jobs := Package.Map.add package job !prep_jobs;
      job
  in
  let get_prep_node package =
    get_prep_job package
    (* |> Option.map @@ fun (compile, monitor) ->
      let node =
        Html.v ~generation ~config
          ~name:(package |> Package.opam |> OpamPackage.to_string)
          ~voodoo:voodoo_gen compile
      in
      let monitor =
        match monitor with
        | Monitor.Seq lst -> Monitor.Seq (lst @ [ ("do-html", Item node) ])
        | _ -> assert false
      in
      let package_status = Monitor.pipeline_state monitor in
      let _index =
        (* let+ step_list = summarise "" [] monitor  *)
        (* DEBUGGING THE MEMORY BLOAT -- Skip the summarising for now *)
        let+ pipeline_id in
        Index.record package pipeline_id package_status []
      in
      (node, monitor) *)
  in
  Package.Set.iter (fun x -> ignore (get_prep_node x)) all;
  !prep_jobs

let blacklist =
  [
    (* "ocaml-secondary-compiler"; *)
    (* "ocamlfind-secondary"; *)
    "ocaml-src";
    "ocaml-freestanding";
    "dkml-component-staging-ocamlrun";
    "dkml-component-offline-ocamlrun";
  ]

let rec with_context_fold lst fn =
  match lst with
  | [] -> fn ()
  | v :: next -> Current.with_context v (fun () -> with_context_fold next fn)

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let group_by criteria list =
  let groups = ref StringMap.empty in
  List.iter
    (fun (pkg, value) ->
      groups :=
        StringMap.update (criteria pkg)
          (function
            | None -> Some [ (pkg, value) ] | Some v -> Some ((pkg, value) :: v))
          !groups)
    list;
  !groups

let collapse_by ~key ~input ?(force_collapse = false) (criteria : 'k -> string)
    (parent : ('k -> string) option) (list : ('k * 'v Current.t) list) :
    ('k * 'v Current.t) list =
  group_by criteria list
  |> StringMap.mapi (fun k v ->
         let curr = List.map snd v in
         let keys = List.map fst v in
         let number_of_groups =
           match parent with
           | None -> List.length v
           | Some parent ->
               List.rev_map parent keys
               |> List.sort_uniq String.compare
               |> List.length
         in
         if number_of_groups <= 1 && not force_collapse then
           List.combine keys curr
         else
           let current, _ =
             Current.collapse_list ~key:(key ^ " " ^ k) ~value:"" ~input curr
           in
           List.combine keys current)
  |> StringMap.bindings
  |> List.rev_map snd
  |> List.flatten

let collapse_single ~key ~input list =
  let curr = List.map snd list in
  let keys = List.map fst list in
  let current, node = Current.collapse_list ~key ~value:"" ~input curr in
  (List.combine keys current, node)

let compile_hierarchical_collapse ~input lst =
  let package_universe = Fmt.to_to_string Package.pp in
  let package_name_version x = x |> Package.opam |> OpamPackage.to_string in
  let package_name x = x |> Package.opam |> OpamPackage.name_to_string in
  let first_char x =
    let name = x |> Package.opam |> OpamPackage.name_to_string in
    String.sub name 0 1 |> String.uppercase_ascii
  in
  let key = "compile" in
  lst
  |> collapse_by ~key ~input ~force_collapse:true package_universe None
  |> collapse_by ~key ~input package_name_version (Some package_universe)
  |> collapse_by ~key ~input package_name (Some package_name_version)
  |> collapse_by ~key ~input first_char (Some package_name)
  |> collapse_single ~key ~input

let v ~config ~opam ~monitor ~migrations () =
  let open Current.Syntax in
  let ssh = Config.ssh config in
  let migrations =
    match migrations with
    | Some path -> Index.migrate path
    | None -> Current.return ()
  in
  let generation = Epoch.v config in
  (* 0) Housekeeping - run migrations *)
  let* _ = migrations in
  Log.info (fun f -> f "0) Migrations");

  (* 1) Track the list of packages in the opam repository *)
  let tracked =
    Track.v
      ~limit:(Config.take_n_last_versions config)
      ~filter:(Config.track_packages config)
      opam
  in
  Log.info (fun f -> f "1) Tracked");
  (* 2) For each package.version, call the solver. *)
  let solver_result_c = Solver.incremental ~config ~blacklist ~opam tracked in
  let* solver_result = solver_result_c in
  Log.info (fun f -> f "2) Solver result");
  (* 3.a) From solver results, obtain a list of package.version.universe corresponding to prep jobs *)
  let all_packages_jobs =
    solver_result
    |> Solver.keys
    |> List.filter_map (fun x -> try Some (Solver.get x) with _ -> None)
  in
  Log.info (fun f -> f "2.5) Solver result...");
  (* 3.b) Expand that list to all the obtainable package.version.universe *)
  let all_packages =
    (* TODO add a append-only layer at this step *)
    all_packages_jobs
    |> List.rev_map Package.all_deps
    |> List.flatten
    |> List.filter (fun pkg ->
           Ocaml_version.compare
             (Package.ocaml_version pkg)
             Ocaml_version.Releases.v4_04_2
           >= 0)
    |> Package.Set.of_list
  in
  Log.info (fun f ->
      f "3) All packages (%d)" (Package.Set.cardinal all_packages));

  (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  (* 4a) Decide on a docker tag for each job *)
  (* 5) Run the preparation step *)
  let weekly = Current_cache.Schedule.v ~valid_for:(Duration.of_day 7) () in

  let repo_opam =
    Current_git.clone ~schedule:weekly
      "https://github.com/ocaml/opam-repository.git"
  in

  let opamfiles =
    Current.component "opamfiles"
    |>
    let> repo_opam in
    let packages =
      Package.Set.to_list all_packages
      |> List.map Package.opam
      |> OpamPackage.Set.of_list
    in
    let extra =
      [
        "conf-graphviz.0.1";
        "conf-which.1";
        "base-threads.base";
        "ocaml-options-vanilla.1";
        "base-bigarray.base";
        "base-domains.base";
        "base-nnp.base";
        "base-effects.base";
        "host-arch-x86_64.1";
        "host-system-other.1";
      ]
    in
    let packages =
      OpamPackage.Set.union
        (List.map OpamPackage.of_string extra |> OpamPackage.Set.of_list)
        packages
      |> OpamPackage.Set.to_list
    in
    Prep.OpamFilesCache.get No_context
      Prep.OpamFiles.Key.{ repo = repo_opam; packages }
  in

  let counts = count_refs all_packages in

  let counts = Package.Map.to_list counts in

  let tot = List.length counts in
  Printf.printf "Total packages: %d\n" tot;

  let counts = List.sort (fun (_, x) (_, y) -> compare !x !y) counts in

  let (_, hd), tl = (List.hd counts, List.tl counts) in
  let last_count, last_val, count =
    List.fold_left
      (fun (cur_count, cur_val, all) (_, count) ->
        if cur_val = !count then (cur_count + 1, cur_val, all)
        else (1, !count, (cur_val, cur_count) :: all))
      (1, !hd, []) tl
  in

  let all_counts = (last_val, last_count) :: count in
  List.iter (fun (v, count) -> Printf.printf "%d %d\n%!" v count) all_counts;

  let cache_packages =
    List.fold_left
      (fun acc (v, count) ->
        if !count > Config.cache_threshold config then Package.Set.add v acc
        else acc)
      Package.Set.empty counts
  in

  Package.add_important_packages cache_packages;

  Log.info (fun f ->
      f "4) Cache packages (%d)" (Package.Set.cardinal cache_packages));

  (* let cumulative =
    let x = ref 0 in
    List.map (fun (v, count) ->
      let y = !x + count in
      x := y;
      (v, y)) all_counts in

  List.iter (fun (v, count) ->
    Printf.printf "%d %d\n%!" v count) cumulative;
    
  ignore @@ exit 0; *)
  let prepped' : Prep.t compile_job Package.Map.t =
    prep ~config ~opamfiles all_packages
  in
  Log.info (fun f -> f ".. %d prepped nodes" (Package.Map.cardinal prepped'));

  Log.info (fun f -> f "5) Prep nodes");
  (* 6) Promote packages to the main tree *)
  let blessed =
    let counts =
      let counts_mut = ref Package.Map.empty in
      Package.Set.iter
        (fun package ->
          Package.all_deps package
          |> List.iter (fun dependency ->
                 counts_mut :=
                   Package.Map.update dependency
                     (function Some v -> Some (v + 1) | None -> Some 1)
                     !counts_mut))
        all_packages;
      !counts_mut
    in
    let by_opam_package =
      Package.Map.fold
        (fun package { job = prep; _ } opam_map ->
          let opam = Package.opam package in
          let job =
            let+ job = Current.state ~hidden:true prep in
            (package, job)
          in
          OpamPackage.Map.update opam (List.cons job) [] opam_map)
        prepped' OpamPackage.Map.empty
    in
    by_opam_package
    |> OpamPackage.Map.mapi (fun opam preps ->
           preps
           |> Current.list_seq
           |> Current.map (fun preps ->
                  (* We don't know yet about all preps status so we're optimistic here *)
                  preps
                  |> List.filter_map (function
                       | _, Error (`Msg _) -> None
                       | pkg, (Error (`Active _) | Ok _) -> Some pkg)
                  |> function
                  | [] -> Package.Blessing.Set.empty opam
                  | list -> Package.Blessing.Set.v ~counts list))
  in
  Log.info (fun f -> f "6) Blessed universes");

  (* 7) Odoc compile and html-generate artifacts *)
  let html, html_input_node, package_pipeline_tree, html2 =
    let compile_monitor = compile ~generation ~config ~blessed prepped' in
    let html2 =
      List.map
        (fun (pkg, { job = compile; _ }) -> (pkg, compile))
        compile_monitor
    in
    Log.info (fun f ->
        f ".. %d compilation nodes" (List.length compile_monitor));
    let c, compile_node =
      compile_monitor
      |> List.map (fun (a, { job = b; _ }) -> (a, b))
      |> compile_hierarchical_collapse ~input:solver_result_c
    in
    ( c |> List.to_seq |> Package.Map.of_seq,
      compile_node,
      compile_monitor
      |> List.map (fun (a, { monitor = b; _ }) -> (a, b))
      |> List.to_seq
      |> Package.Map.of_seq,
      html2 |> Package.Map.of_list )
  in
  Log.info (fun f -> f "7) Odoc compile nodes");

  let prep_pipeline_tree =
    Package.Map.map (fun (p : Prep.t compile_job) -> p.monitor) prepped'
  in

  (* 7.b) Inform the monitor *)
  let () =
    let solver_failures = Solver.failures solver_result in
    let successes = List.length (Solver.keys solver_result) in
    Log.info (fun f ->
        f "7.b) Inform the monitor: successes %i, failures %i" successes
          (List.length solver_failures));
    Monitor.register monitor solver_failures prep_pipeline_tree blessed
      package_pipeline_tree html2
  in
  Log.info (fun f -> f "7.b) Inform monitor");

  (* 8) Update live folders *)
  let live_branch =
    let generation = Current.return ~label:"Generation" generation in
    Current.collapse ~input:html_input_node ~key:"Update live folders" ~value:""
    @@
    let commits_raw =
      Package.Map.bindings html
      |> List.map (fun (_, html_current) ->
             html_current
             |> Current.map (fun t -> (Compile.hashes t).html_hash)
             |> Current.state ~hidden:true)
      |> Current.list_seq
      |> Current.map (List.filter_map Result.to_option)
    in
    let live_html = Live.set_to ~ssh "html" generation in
    let live_linked = Live.set_to ~ssh "linked" generation in
    Current.all [ commits_raw |> Current.ignore_value; live_html; live_linked ]
  in
  Log.info (fun f -> f "8) Pipeline ready");
  live_branch
