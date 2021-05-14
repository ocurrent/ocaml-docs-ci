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

let compile ~config ~voodoo ~cache ~(blessed : Package.Blessed.t Current.t)
    (preps : Prep.t Current.t Package.Map.t) =
  let compilation_jobs = ref Package.Map.empty in

  let rec get_compilation_job package =
    try Package.Map.find package !compilation_jobs
    with Not_found ->
      let job =
        Package.Map.find_opt package preps
        |> Option.map @@ fun prep ->
           let dependencies = Package.universe package |> Package.Universe.deps in
           let compile_dependencies =
             List.filter_map get_compilation_job dependencies |> Current.list_seq
           in
           Compile.v ~config ~cache ~voodoo ~blessed ~deps:compile_dependencies prep
      in
      compilation_jobs := Package.Map.add package job !compilation_jobs;
      job
  in
  let get_compilation_node package _ = get_compilation_job package in
  Package.Map.filter_map get_compilation_node preps |> Package.Map.bindings

let blacklist = [ "ocaml-secondary-compiler"; "ocamlfind-secondary" ]

let take_any_success jobs =
  let open Current.Syntax in
  let* statuses = jobs |> List.map (Current.state ~hidden:true) |> Current.list_seq in
  let to_int = function
    | Ok _ -> 4
    | Error (`Active `Running) -> 3
    | Error (`Active `Ready) -> 2
    | Error (`Msg _) -> 1
  in
  let max a b = if to_int a >= to_int b then a else b in
  List.fold_left max (List.hd statuses) (List.tl statuses) |> Current.of_output

module StringMap = Map.Make (String)

let collapse_by ~key ~input (criteria : 'k -> string) (list : ('k * 'v Current.t) list) :
    ('k * 'v Current.t) list =
  let groups = ref StringMap.empty in
  List.iter
    (fun (pkg, value) ->
      groups :=
        StringMap.update (criteria pkg)
          (function None -> Some [ (pkg, value) ] | Some v -> Some ((pkg, value) :: v))
          !groups)
    list;

  !groups
  |> StringMap.mapi (fun k v ->
         let curr = List.map snd v in
         let keys = List.map fst v in
         let current = Current.collapse_list ~key:(key ^ " " ^ k) ~value:"" ~input curr in
         List.combine keys current)
  |> StringMap.bindings |> List.rev_map snd |> List.flatten

let prep_hierarchical_collapse ~input lst =
  let key = "prep" in
  lst
  |> collapse_by ~key ~input (fun x -> x.Jobs.install |> Package.opam |> OpamPackage.name_to_string)
  |> collapse_by ~key ~input (fun x ->
         let name = x.Jobs.install |> Package.opam |> OpamPackage.name_to_string in
         String.sub name 0 1 |> String.uppercase_ascii)
  |> collapse_by ~key ~input (fun _ -> "")

let compile_hierarchical_collapse ~input lst =
  let key = "compile" in
  lst
  |> collapse_by ~key ~input (Fmt.to_to_string Package.pp)
  |> collapse_by ~key ~input (fun x -> x |> Package.opam |> OpamPackage.to_string)
  |> collapse_by ~key ~input (fun x -> x |> Package.opam |> OpamPackage.name_to_string)
  |> collapse_by ~key ~input (fun x ->
         let name = x |> Package.opam |> OpamPackage.name_to_string in
         String.sub name 0 1 |> String.uppercase_ascii)
  |> collapse_by ~key ~input (fun _ -> "")

let v ~config ~api ~opam () =
  let open Current.Syntax in
  let cache = Remote_cache.v (Config.ssh config) in
  let voodoo = Voodoo.v config in
  let v_do = Current.map Voodoo.Do.v voodoo in
  let v_prep = Current.map Voodoo.Prep.v voodoo in
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
    all_packages_jobs |> List.rev_map Package.all_deps |> List.flatten
  in
  (* 4) Schedule a somewhat small set of jobs to obtain at least one universe for each package.version *)
  let jobs = Jobs.schedule ~targets:all_packages all_packages_jobs in
  (* 5) Run the preparation step *)
  let prepped =
    jobs
    |> List.map (fun job -> (job, Prep.v ~config ~cache ~voodoo:v_prep job))
    |> prep_hierarchical_collapse ~input:solver_result
    |> List.map (fun (job, result) ->
           job.Jobs.prep |> List.to_seq
           |> Seq.map (fun p -> (p, [ Current.map (Package.Map.find p) result ]))
           |> Package.Map.of_seq)
    |> List.fold_left (Package.Map.union (fun _ a b -> Some (a @ b))) Package.Map.empty
    |> Package.Map.map take_any_success
  in
  let prep_list =
    Package.Map.bindings prepped
    |> List.rev_map (fun (package, prep) ->
           let+ prep = Current.state ~hidden:true prep in
           (package, prep))
    |> Current.list_seq
  in
  (* 6) Promote packages to the main tree *)
  let blessed =
    let+ preps = prep_list in
    (* We don't know yet about all preps status so we're optimistic here *)
    preps
    |> List.filter_map (function
         | _, Error (`Msg _) -> None
         | pkg, (Error (`Active _) | Ok _) -> Some pkg)
    |> Package.Blessed.v
  in
  (* 7) Odoc compile and html-generate artifacts *)
  let compiled =
    compile ~config ~cache ~voodoo:v_do ~blessed prepped
    |> compile_hierarchical_collapse ~input:blessed
    |> List.rev_map (fun (package, task) -> Current.state ~hidden:true task |> Current.map (fun v -> package, v))
    |> Current.list_seq
  in
  let package_registry =
    let+ tracked = tracked in
    List.iter
      (fun p ->
        Log.app (fun f -> f "Track: %s" (OpamPackage.to_string (Track.pkg p)));
        Web.register (Track.pkg p) api)
      tracked
  in
  let package_status =
    let+ compiled = compiled and+ prepped = prep_list and+ blessed = blessed in
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
        | package, Error (`Msg _) -> (set_if_better Failed) package
        | package, Error (`Active _) -> (set_if_better (Pending Prep)) package)
      prepped;
    List.iter
      (fun (package, status) ->
        let blessed = if Package.Blessed.is_blessed blessed package then Docs_ci_lib.Web.Status.Blessed else Universe in
        match status with
        | Ok _ -> set (Success blessed) package
        | Error (`Msg _) -> set Failed package
        | Error (`Active _) -> set (Pending (Compile blessed)) package)
      compiled;
    !status |> Package.Map.bindings
  in
  let status =
    let* _s = package_status
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
    |> Current.collapse ~key:"status-update" ~value:"" ~input:package_status in
    Current.return ()
  in
  let status2 =
    let* status = package_status in
    let package_versions = Hashtbl.create 1000 in
    List.iter (fun (package, status) ->
      let name = Package.opam package |> OpamPackage.name_to_string in
      let version = Package.opam package |> OpamPackage.version_to_string in
      match Hashtbl.find_opt package_versions name with
      | Some vs ->
        Hashtbl.replace package_versions name ((version, status)::vs)
      | None ->
        Hashtbl.add package_versions name [(version, status)]) status;
    let ssh = Config.ssh config in
    let vs =
      Hashtbl.fold (fun k v acc -> (Indexes.v ~ssh ~package_name:k ~statuses:(Current.return v)) :: acc) package_versions []
    in
    Current.all vs
  in      
  Current.all [package_registry; status; status2]

