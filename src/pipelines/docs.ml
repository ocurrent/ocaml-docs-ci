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

module PrepStatus = struct
  type t = Jobs.t * Prep.t list Current_term.Output.t

  let pp f (t, _) = Fmt.pf f "Prep status %a" Jobs.pp t

  let compare (j1, r1) (j2, r2) =
    match Jobs.compare j1 j2 with
    | 0 ->
        Result.compare
          ~ok:(fun _ _ -> 0 (*twp same jobs yield the same prep list*))
          ~error:Stdlib.compare r1 r2
    | v -> v
end

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

let compile ~generation ~config ~voodoo_gen ~voodoo_do
    ~(blessed : Package.Blessing.Set.t Current.t OpamPackage.Map.t) ~pipeline_id
    (preps : (Prep.prep Current.t * Prep.t Current.t) Package.Map.t) =
  let compilation_jobs = ref Package.Map.empty in

  let rec get_compilation_job package =
    let name = package |> Package.opam |> OpamPackage.to_string in
    try Package.Map.find package !compilation_jobs
    with Not_found ->
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun (prep_node, prep) ->
           let dependencies =
             Package.universe package |> Package.Universe.deps
           in
           let compile_dependencies_names =
             List.filter_map
               (fun p ->
                 get_compilation_job p |> Option.map (fun (a, _) -> (p, a)))
               dependencies
           in
           let compile_dependencies =
             compile_dependencies_names |> List.map snd
           in
           let blessing =
             OpamPackage.Map.find (Package.opam package) blessed
             |> Current.map (fun b -> Package.Blessing.Set.get b package)
           in
           let node =
             Compile.v ~generation ~config ~name ~voodoo:voodoo_do ~blessing
               ~deps:(Current.list_seq compile_dependencies)
               prep
           in
           let monitor =
             Monitor.(
               Seq
                 [
                   ( "do-deps",
                     And
                       (("prep", Item prep_node)
                       :: List.map
                            (fun (pkg, compile) ->
                              let dep_prep_node, _ =
                                Package.Map.find pkg preps
                              in
                              ( "dep-compile " ^ Package.id pkg,
                                And
                                  [
                                    ("prep", Item dep_prep_node);
                                    ("compile", Item compile);
                                  ] ))
                            compile_dependencies_names) );
                   ("do-compile", Item node);
                 ])
           in
           (node, monitor)
      in
      compilation_jobs := Package.Map.add package job !compilation_jobs;
      job
  in
  let get_compilation_node package _ =
    get_compilation_job package
    |> Option.map @@ fun (compile, monitor) ->
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
       (node, monitor)
  in
  Package.Map.filter_map get_compilation_node preps |> Package.Map.bindings

let blacklist =
  [
    "ocaml-secondary-compiler";
    "ocamlfind-secondary";
    "ocaml-src";
    "ocaml-freestanding";
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

let prep_hierarchical_collapse ~input lst =
  let package_name x =
    x.Jobs.install |> Package.opam |> OpamPackage.name_to_string
  in
  let first_char x =
    let name = x.Jobs.install |> Package.opam |> OpamPackage.name_to_string in
    String.sub name 0 1 |> String.uppercase_ascii
  in
  let key = "prep" in
  lst
  |> collapse_by ~key ~input ~force_collapse:true package_name None
  |> collapse_by ~key ~input first_char (Some package_name)
  |> collapse_single ~key ~input

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
  let voodoo = Voodoo.v config in
  let ssh = Config.ssh config in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_gen = Current.map Voodoo.Gen.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
  let migrations =
    match migrations with
    | Some path -> Index.migrate path
    | None -> Current.return ()
  in
  let voodoo =
    let+ _ = migrations and+ voodoo in
    voodoo
  in
  let generation =
    let+ voodoo in
    Epoch.v config voodoo
  in
  (* 0) Housekeeping - run migrations, record a new pipeline run *)
  let* _ = migrations in
  Log.info (fun f -> f "0) Migrations");

  let pipeline_id = Record.v config voodoo in
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
    solver_result |> Solver.keys |> List.rev_map Solver.get
  in
  (* 3.b) Expand that list to all the obtainable package.version.universe *)
  let all_packages =
    (* TODO add a append-only layer at this step *)
    all_packages_jobs
    |> List.rev_map Package.all_deps
    |> List.flatten
    |> Package.Set.of_list
  in
  Log.info (fun f -> f "3) All packages");
  (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  let jobs = Jobs.schedule ~targets:all_packages all_packages_jobs in
  (* 4a) Decide on a docker tag for each job *)
  let jobs' = List.map (fun job -> (job, Misc.spec_of_job job)) jobs in
  Log.info (fun f -> f "4) Jobs are scheduled");
  (* 5) Run the preparation step *)
  let prepped =
    jobs'
    |> List.map (fun (job, spec) ->
           (job, Prep.v ~config ~voodoo:v_prep ~spec job))
  in
  let prepped' =
    prepped
    |> List.map (fun (job, result) ->
           Prep.extract ~job result |> Package.Map.map (fun v -> (result, v)))
    |> List.fold_left
         (Package.Map.union (fun _ _ _ ->
              failwith "Two jobs prepare the same package."))
         Package.Map.empty
  in
  let prep_nodes =
    Package.Map.fold
      (fun package (prep_job, _) opam_map ->
        let opam = Package.opam package in
        OpamPackage.Map.update opam (List.cons (package, prep_job)) [] opam_map)
      prepped' OpamPackage.Map.empty
  in
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
        (fun package (_, job) opam_map ->
          let opam = Package.opam package in
          let job =
            let+ job = Current.state ~hidden:true job in
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
  let html, html_input_node, package_pipeline_tree =
    let compile_monitor =
      compile ~generation ~config ~voodoo_do:v_do ~voodoo_gen:v_gen ~blessed
        ~pipeline_id prepped'
    in
    Log.info (fun f ->
        f ".. %d compilation nodes" (List.length compile_monitor));
    let c, compile_node =
      compile_monitor
      |> List.map (fun (a, (b, _)) -> (a, b))
      |> compile_hierarchical_collapse ~input:solver_result_c
    in
    ( c |> List.to_seq |> Package.Map.of_seq,
      compile_node,
      compile_monitor
      |> List.map (fun (a, (_, b)) -> (a, b))
      |> List.to_seq
      |> Package.Map.of_seq )
  in
  Log.info (fun f -> f "7) Odoc compile nodes");

  (* 7.b) Inform the monitor *)
  let () =
    let solver_failures = Solver.failures solver_result in
    let successes = List.length (Solver.keys solver_result) in
    Log.info (fun f ->
        f "7.b) Inform the monitor: successes %i, failures %i" successes
          (List.length solver_failures));
    Monitor.register monitor solver_failures prep_nodes blessed
      package_pipeline_tree
  in
  Log.info (fun f -> f "7.b) Inform monitor");

  (* 8) Update live folders *)
  let live_branch =
    Current.collapse ~input:html_input_node ~key:"Update live folders" ~value:""
    @@
    let commits_raw =
      Package.Map.bindings html
      |> List.map (fun (_, html_current) ->
             html_current
             |> Current.map (fun t -> (Html.hashes t).html_raw_hash)
             |> Current.state ~hidden:true)
      |> Current.list_seq
      |> Current.map (List.filter_map Result.to_option)
    in
    let live_html = Live.set_to ~ssh "html" `Html generation in
    let live_linked = Live.set_to ~ssh "linked" `Linked generation in
    Current.all [ commits_raw |> Current.ignore_value; live_html; live_linked ]
  in
  Log.info (fun f -> f "8) Pipeline ready");
  live_branch
