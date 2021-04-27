open Docs_ci_lib

module ListMap (M : Map.S) = struct
  include M

  type 'v t = 'v list M.t

  let of_list bindings =
    let t = ref M.empty in
    List.iter
      (fun (k, v) ->
        t :=
          match M.find_opt k !t with None -> M.add k [ v ] !t | Some lst -> M.add k (v :: lst) !t)
      bindings;
    !t

  let values t = t |> bindings |> List.map snd
end

module CharListMap = ListMap (Map.Make (Char))
module NameListMap = ListMap (Map.Make (OpamPackage.Name))

let compile ~voodoo ~cache ~(blessed : Package.Blessed.t Current.t) (preps : Prep.t list Current.t)
    =
  let open Current.Syntax in
  let preps_current = preps in
  (* this bind is heavy, and we expect the preps list to update a lot *)
  let* preps = preps in
  let pkg_preps =
    List.map (fun prep -> (Prep.package prep, prep)) preps |> List.to_seq |> Package.Map.of_seq
  in
  let jobs : Compile.t Current.t option Package.Map.t ref = ref Package.Map.empty in
  let rec get_compilation_job pkg =
    match Package.Map.find_opt pkg !jobs with
    | Some job -> job
    | None ->
        let deps =
          pkg |> Package.universe |> Package.Universe.deps
          |> List.filter_map get_compilation_job
          |> Current.list_seq
        in
        let result =
          Package.Map.find_opt pkg pkg_preps
          |> Option.map (fun prep ->
                 prep |> Current.return |> Compile.v ~cache ~voodoo ~blessed ~deps)
        in
        jobs := Package.Map.add pkg result !jobs;
        result
  in
  preps
  |> List.sort_uniq (fun a b -> Package.compare (Prep.package a) (Prep.package b))
  |> List.filter_map (fun prep ->
         let package = Prep.package prep in
         get_compilation_job package
         |> Option.map (fun x ->
                ( (package |> Package.opam |> OpamPackage.name_to_string).[0] |> Char.uppercase_ascii,
                  ( package |> Package.opam |> OpamPackage.name,
                    x |> Current.state ~hidden:true |> Current.pair blessed
                    |> Current.map (fun (blessed, x) ->
                           (prep, Package.Blessed.is_blessed blessed package, x))
                    |> Current.collapse
                         ~key:(package |> Package.opam |> OpamPackage.to_string)
                         ~value:"" ~input:preps_current ) )))
  |> CharListMap.of_list
  |> CharListMap.mapi (fun c names_versions ->
         names_versions |> NameListMap.of_list
         |> NameListMap.mapi (fun name versions ->
                versions |> Current.list_seq
                |> Current.collapse ~key:(OpamPackage.Name.to_string name) ~value:""
                     ~input:preps_current)
         |> NameListMap.values |> Current.list_seq
         |> Current.collapse ~key:(String.make 1 c) ~value:"" ~input:preps_current)
  |> CharListMap.values |> Current.list_seq
  |> Current.map (fun l -> l |> List.flatten |> List.flatten)

let blacklist = [ "ocaml-secondary-compiler"; "ocamlfind-secondary" ]

let v ~api ~opam () =
  let open Current.Syntax in
  let cache = Remote_cache.v () in
  let voodoo = Voodoo.v () in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
  let solver_result =
    (* 1) Track the list of packages in the opam repository *)
    let tracked = Track.v ~filter:Config.track_packages opam in
    (* 2) For each package.version, call the solver.  *)
    Solver.incremental ~blacklist ~opam tracked
  in
  (* 3.a) From solver results, obtain a list of package.version.universe corresponding to prep jobs *)
  let all_packages_jobs =
    solver_result |> Current.map (fun r -> Solver.keys r |> List.rev_map Solver.get)
  in
  (* 3.b) Expand that list to all the obtainable package.version.universe *)
  let all_packages =
    (* todo: add a append-only layer at this step *)
    all_packages_jobs |> Current.map (List.rev_map Package.all_deps) |> Current.map List.flatten
  in
  let prepped =
    (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
    let jobs =
      let+ targets = all_packages and+ all_packages_jobs = all_packages_jobs in
      Jobs.schedule ~targets all_packages_jobs
    in
    (* 5) Run the preparation step *)
    Current.collapse ~key:"prep" ~value:"" ~input:jobs
    @@ Current.list_map
         (module Jobs)
         (fun job ->
           Prep.v ~cache ~voodoo:v_prep job |> Current.state ~hidden:true |> Current.pair job)
         jobs
  in
  (* 6) Promote packages to the main tree *)
  let blessed =
    Current.map
      (fun prep_status ->
        (* We don't know yet about all preps so *)
        prep_status
        |> List.filter_map (function
             | _, Error (`Msg _) -> None
             | _, Ok prep -> Some (List.map Prep.package prep)
             | Jobs.{ prep; _ }, _ -> Some prep)
        |> List.flatten |> Package.Blessed.v)
      prepped
  in

  (* 7) Odoc compile and html-generate artifacts *)
  let compiled =
    let prepped =
      Current.map
        (fun x -> x |> List.filter_map (fun (_, b) -> Result.to_option b) |> List.flatten)
        prepped
    in
    compile ~cache ~voodoo:v_do ~blessed prepped
  in
  let package_status =
    let+ compiled = compiled and+ prepped = prepped in
    let status = ref Package.Map.empty in
    let set value package = status := Package.Map.add package value !status in
    let set_if_better value package =
      match Package.Map.find_opt package !status with
      | None -> set value package
      | Some v when Web.Status.compare v value < 0 -> set value package
      | _ -> ()
    in
    List.iter
      (function
        | _, Ok _ -> ()
        | Jobs.{ prep; _ }, Error (`Msg _) -> List.iter (set_if_better Failed) prep
        | Jobs.{ prep; _ }, Error (`Active _) -> List.iter (set_if_better (Pending Prep)) prep)
      prepped;
    List.iter
      (fun (prep, blessed, status) ->
        let package = Prep.package prep in
        let blessed = if blessed then Docs_ci_lib.Web.Status.Blessed else Universe in
        match status with
        | Ok _ -> set (Success blessed) package
        | Error (`Msg _) -> set Failed package
        | Error (`Active _) -> set (Pending (Compile blessed)) package)
      compiled;
    !status |> Package.Map.bindings
  in
  package_status
  |> Current.list_map
       ( module struct
         type t = Package.t * Web.Status.t

         let compare (a1, a2) (b1, b2) =
           match Package.compare a1 b1 with 0 -> Web.Status.compare a2 b2 | v -> v

         let pp f (package, status) = Fmt.pf f "%a -> %a" Package.pp package Web.Status.pp status
       end )
       (fun pkg_value ->
         let package = Current.map fst pkg_value in
         let status = Current.map snd pkg_value in
         Web.set_package_status ~package ~status api)
  |> Current.collapse ~key:"status-update" ~value:"" ~input:package_status
