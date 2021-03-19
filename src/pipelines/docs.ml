open Docs_ci_lib

let compile ~(blessed : Package.Blessed.t Current.t) (preps : Prep.t list Current.t) =
  let open Current.Syntax in
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
          |> Option.map (fun prep -> prep |> Current.return |> Compile.v ~blessed ~deps)
        in
        jobs := Package.Map.add pkg result !jobs;
        result
  in
  List.filter_map (fun prep -> prep |> Prep.package |> get_compilation_job) preps
  |> Current.list_seq

let v ~opam () =
  let open Docs in
  let open Current.Syntax in
  let* opam = opam in
  let all_packages_jobs =
    let tracked = track ~filter:[ "uri"; "result" ] (Current.return opam) in
    Current.collapse ~key:"solve" ~value:"" ~input:tracked
      (Current.list_map (module O.OpamPackage) (fun opam_pkg -> solve ~opam opam_pkg) tracked)
  in
  let all_packages =
    (* todo: add a append-only layer at this step *)
    all_packages_jobs |> Current.map (List.map Package.all_deps) |> Current.map List.flatten
  in
  let prepped =
    let jobs = select_jobs ~targets:all_packages in
    Current.collapse ~key:"prep" ~value:"" ~input:jobs
    @@ let+ res =
         Current.list_map (module Jobs) (fun job -> Prep.v job |> Current.catch ~hidden:true) jobs
       in
       List.filter_map Result.to_option res |> List.flatten
  in
  let blessed =
    Current.map (fun prep -> prep |> List.map Prep.package |> Package.Blessed.v) prepped
  in
  let compiled = compile ~blessed prepped in
  Indexes.v compiled

(*
  assemble_and_link (Current.map (List.map snd) prepped) compiled
*)
