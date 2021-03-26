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

let compile ~voodoo ~(blessed : Package.Blessed.t Current.t) (preps : Prep.t list Current.t) =
  let open Current.Syntax in
  let preps_current = preps in
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
          |> Option.map (fun prep -> prep |> Current.return |> Compile.v ~voodoo ~blessed ~deps)
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
                    x (*|> Current.state ~hidden:true*)
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

let v ~opam () =
  let open Current.Syntax in
  let voodoo = Voodoo.v in
  let all_packages_jobs =
    let tracked = Track.v ~filter:[ "result"; "mirage"; "cohttp"; "irmin" ] opam in
    Solver.incremental ~blacklist ~opam tracked
  in
  let all_packages =
    (* todo: add a append-only layer at this step *)
    all_packages_jobs |> Current.map (List.map Package.all_deps) |> Current.map List.flatten
  in
  let prepped =
    let jobs =
      let+ targets = all_packages and+ all_packages_jobs = all_packages_jobs in
      Jobs.schedule ~targets all_packages_jobs
    in
    Current.collapse ~key:"prep" ~value:"" ~input:jobs
    @@ let+ res =
         Current.list_map
           (module Jobs)
           (fun job -> Prep.v ~voodoo job |> Current.catch ~hidden:true)
           jobs
       in
       List.filter_map Result.to_option res |> List.flatten
  in
  let blessed =
    Current.map (fun prep -> prep |> List.map Prep.package |> Package.Blessed.v) prepped
  in
  let compiled = compile ~voodoo ~blessed prepped in
  Indexes.v compiled
