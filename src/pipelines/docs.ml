open Docs_ci_lib

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

let get_compilation_job ~preps ~voodoo ~cache ~blessed pkg =
  let v = ref Package.Map.empty in
  let rec aux_get_job package =
    match Package.Map.find_opt package !v with
    | Some v -> v
    | None ->
        v := Package.Map.add package None !v;
        let deps =
          package |> Package.universe |> Package.Universe.deps |> List.filter_map aux_get_job
          |> Current.list_seq
        in
        let result =
          match Package.Map.find_opt package preps with
          | Some prep -> Some (Compile.v ~cache ~voodoo ~blessed ~deps (Current.return prep))
          | None -> None
        in
        v := Package.Map.add package result !v;
        result
  in
  match aux_get_job pkg with Some v -> Current.map Option.some v | None -> Current.return None

let compile ~voodoo ~cache ~input ~(blessed : Package.Blessed.t Current.t)
    (preps : PrepStatus.t list Current.t) =
  let open Current.Syntax in
  let valid_preps =
    Current.collapse ~key:"get preps status" ~value:"" ~input
    @@ Current.list_map
         (module PrepStatus)
         (fun prep_status ->
           let+ _, status = prep_status in
           match status with Ok preps -> preps | _ -> [])
         preps
    |> Current.map (fun x ->
           x |> List.flatten
           |> List.sort_uniq (fun a b -> Package.compare (Prep.package a) (Prep.package b)))
  in
  let pool = Compile.Pool.v () in
  Current.list_map
    (module Prep)
    (fun prep ->
      let deps = Compile.Monitor.v pool prep in
      Compile.v ~cache ~voodoo ~blessed ~deps prep
      |> Current.state ~hidden:true |> Current.pair prep
      |> Current.map (fun (prep, x) ->
             let package = Prep.package prep in
             Compile.Pool.update pool package x;
             (package, x))
      |> Current.pair blessed
      |> Current.map (fun (blessed, (package, x)) ->
             (package, Package.Blessed.is_blessed blessed package, x)))
    valid_preps
  |> Current.collapse ~key:"compile" ~value:"" ~input:valid_preps

let blacklist = [ "ocaml-secondary-compiler"; "ocamlfind-secondary" ]

let v ~api ~opam () =
  let open Current.Syntax in
  let cache = Remote_cache.v () in
  let voodoo = Voodoo.v () in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
  (* 1) Track the list of packages in the opam repository *)
  let tracked = Track.v ~filter:Config.track_packages opam in
  (* 2) For each package.version, call the solver.  *)
  let solver_result = Solver.incremental ~blacklist ~opam tracked in
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
    Current.with_context v_prep @@ fun () ->
    Current.with_context cache @@ fun () ->
    Current.collapse ~key:"preps" ~value:"" ~input:jobs
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
        (* We don't know yet about all preps status so we're optimistic here *)
        prep_status
        |> List.filter_map (function
             | _, Error (`Msg _) -> None
             | _, Ok prep -> Some (List.map Prep.package prep)
             | Jobs.{ prep; _ }, _ -> Some prep)
        |> List.flatten |> Package.Blessed.v)
      prepped
  in
  (* 7) Odoc compile and html-generate artifacts *)
  let compiled = compile ~input:blessed ~cache ~voodoo:v_do ~blessed prepped in
  let package_registry =
    let+ tracked = tracked in

    List.iter
      (fun p ->
        Log.app (fun f -> f "Track: %s" (OpamPackage.to_string (Track.pkg p)));
        Web.register (Track.pkg p) api)
      tracked
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
      (fun (package, blessed, status) ->
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

         let pp f (package, status) = Fmt.pf f "%a => %a" Package.pp package Web.Status.pp status
       end )
       (fun pkg_value ->
         let package = Current.map fst pkg_value in
         let status = Current.map snd pkg_value in
         Web.set_package_status ~package ~status api)
  |> Current.collapse ~key:"status-update" ~value:"" ~input:package_status
  |> Current.pair package_registry
