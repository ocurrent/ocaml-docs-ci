open Docs_ci_lib

module CurrentMap (Map : OpamStd.MAP) = struct
  let map_seq (input : 'a Current.t Map.t) : 'a Map.t Current.t =
    Map.bindings input
    |> List.rev_map (fun (k, v) -> Current.map (fun x -> (k, x)) v)
    |> Current.list_seq |> Current.map Map.of_list
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

let compile ~generation ~config ~voodoo_gen ~voodoo_do
    ~(blessed : Package.Blessing.Set.t Current.t OpamPackage.Map.t)
    (preps : (Prep.prep Current.t * Prep.t Current.t) Package.Map.t) =
  let compilation_jobs = ref Package.Map.empty in

  let rec get_compilation_job package =
    let name = package |> Package.opam |> OpamPackage.to_string in
    try Package.Map.find package !compilation_jobs
    with Not_found ->
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun (prep_node, prep) ->
           let dependencies = Package.universe package |> Package.Universe.deps 
           in
           let compile_dependencies_names =
            List.filter_map (fun p -> 
              get_compilation_job p
              |> Option.map (fun (a, _) -> p, a)) 
            dependencies
           in
           let compile_dependencies =
            compile_dependencies_names
            |> List.map snd
           in
           let blessing =
             OpamPackage.Map.find (Package.opam package) blessed
             |> Current.map (fun b -> Package.Blessing.Set.get b package)
           in
           let node = 
            Compile.v ~generation ~config ~name ~voodoo:voodoo_do ~blessing
              ~deps:(Current.list_seq compile_dependencies) prep
           in
           let monitor = 
            Monitor.(
            Seq [
              ("do-deps", And
                (("prep", Item prep_node) ::
                List.map (fun (pkg, compile) -> 
                  ("dep-compile " ^ Package.id pkg, Item compile)) 
                  compile_dependencies_names)
              );
              ("do-compile", Item node)
            ]
            )
           in
           node, monitor
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
      | Monitor.Seq lst ->
        Monitor.Seq (lst @ ["do-html", Item node])
      | _ -> assert false
    in
    node, monitor
  in
  Package.Map.filter_map get_compilation_node preps |> Package.Map.bindings

let blacklist = [ "ocaml-secondary-compiler"; "ocamlfind-secondary" ]

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
          (function None -> Some [ (pkg, value) ] | Some v -> Some ((pkg, value) :: v))
          !groups)
    list;
  !groups

let collapse_by ~key ~input ?(force_collapse = false) (criteria : 'k -> string)
    (parent : ('k -> string) option) (list : ('k * 'v Current.t) list) : ('k * 'v Current.t) list =
  group_by criteria list
  |> StringMap.mapi (fun k v ->
         let curr = List.map snd v in
         let keys = List.map fst v in
         let number_of_groups =
           match parent with
           | None -> List.length v
           | Some parent -> List.rev_map parent keys |> List.sort_uniq String.compare |> List.length
         in
         if number_of_groups <= 1 && not force_collapse then List.combine keys curr
         else
           let current, _ = Current.collapse_list ~key:(key ^ " " ^ k) ~value:"" ~input curr in
           List.combine keys current)
  |> StringMap.bindings |> List.rev_map snd |> List.flatten

let collapse_single ~key ~input list =
  let curr = List.map snd list in
  let keys = List.map fst list in
  let current, node = Current.collapse_list ~key ~value:"" ~input curr in
  (List.combine keys current, node)

let prep_hierarchical_collapse ~input lst =
  let package_name x = x.Jobs.install |> Package.opam |> OpamPackage.name_to_string in
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

let v ~config ~opam ~monitor () =
  let open Current.Syntax in
  let voodoo = Voodoo.v config in
  let ssh = Config.ssh config in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_gen = Current.map Voodoo.Gen.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
  let generation =
    let+ voodoo = voodoo in
    Epoch.v config voodoo
  in
  (* 1) Track the list of packages in the opam repository *)
  let tracked =
    Track.v ~limit:(Config.take_n_last_versions config) ~filter:(Config.track_packages config) opam
  in
  (* 2) For each package.version, call the solver.  *)
  let solver_result = Solver.incremental ~config ~blacklist ~opam tracked in
  (* 3.a) From solver results, obtain a list of package.version.universe corresponding to prep jobs *)
  let* all_packages_jobs =
    solver_result |> Current.map (fun r -> Solver.keys r |> List.rev_map Solver.get)
  in
  (* 3.b) Expand that list to all the obtainable package.version.universe *)
  let all_packages =
    (* todo: add a append-only layer at this step *)
    all_packages_jobs |> List.rev_map Package.all_deps |> List.flatten |> Package.Set.of_list
  in
  (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  let jobs = Jobs.schedule ~targets:all_packages all_packages_jobs in
  (* 4a) Decide on a docker tag for each job *)
  let jobs' = jobs |> List.map (fun job -> (job, Misc.spec_of_job job)) in
  (* 5) Run the preparation step *)
  let prepped, prepped_input_node =
    jobs'
    |> List.map (fun (job, spec) ->
           ( job,
             let* spec = spec in
             Prep.v ~config ~voodoo:v_prep ~spec job ))
    |> prep_hierarchical_collapse ~input:solver_result
  in
  let prepped' =
    prepped
    |> List.map (fun (job, result) -> 
      Prep.extract ~job result
      |> Package.Map.map (fun v -> result, v))
    |> List.fold_left
         (Package.Map.union (fun _ _ _ -> failwith "Two jobs prepare the same package."))
         Package.Map.empty
  in
  let prep_nodes = 
    Package.Map.fold
      (fun package (prep_job, _) opam_map ->
        let opam = Package.opam package in
        OpamPackage.Map.update opam (List.cons (package, prep_job)) [] opam_map)
      prepped' OpamPackage.Map.empty
  in
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
           preps |> Current.list_seq
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

  (* 7) Odoc compile and html-generate artifacts *)
  let html, html_input_node, package_pipeline_tree =
    let compile_monitor =
      compile ~generation ~config ~voodoo_do:v_do ~voodoo_gen:v_gen ~blessed prepped'
    in
    let c, compile_node =
      compile_monitor
      |> List.map (fun (a, (b, _)) -> (a,b))
      |> compile_hierarchical_collapse ~input:prepped_input_node
    in
    (c |> List.to_seq |> Package.Map.of_seq, 
    compile_node,
    compile_monitor
    |> List.map (fun (a, (_, b)) -> (a,b))
    |> List.to_seq |> Package.Map.of_seq)
  in

  (* 7.b) Inform the monitor *)

  let () =
    Monitor.register monitor prep_nodes blessed package_pipeline_tree
  in

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
  live_branch
