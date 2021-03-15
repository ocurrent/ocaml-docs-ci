open Docs_ci_lib

module OpamPackage = struct
  include OpamPackage

  let pp f t = Fmt.pf f "%s" (to_string t)
end

let v ~opam () =
  let open Docs in
  let open Current.Syntax in
  let* opam = opam in
  let all_packages_jobs =
    let tracked = track ~filter:[ "uri"; "result" ] (Current.return opam) in
    Current.collapse ~key:"solve" ~value:"" ~input:tracked
      (Current.list_map
         (module OpamPackage)
         (fun opam_pkg ->
           let+ packages = solve ~opam opam_pkg and+ opam_pkg = opam_pkg in
           let root = List.find (fun pkg -> Package.opam pkg = opam_pkg) packages in
           { Jobs.root; pkgs = packages })
         tracked)
  in
  let blessed_packages =
    all_packages_jobs
    |> Current.map (List.map (fun x -> x.Jobs.pkgs))
    |> Current.map List.flatten |> bless_packages
  in
  let jobs = select_jobs ~targets:all_packages_jobs ~blessed:blessed_packages in
  let prepped =
    Current.collapse ~key:"prep" ~value:"" ~input:jobs
    @@ let+ res =
         Current.list_map
           (module Jobs)
           (fun job ->
             Current.pair job (build_and_prep (Current.map (fun x -> x.Jobs.root) job))
             |> Current.catch ~hidden:true)
           jobs
       in
       List.filter_map Result.to_option res
  in
  let compiled =
    Current.collapse ~key:"link" ~value:"" ~input:prepped
    @@ Current.list_map
         ( module struct
           type t = Jobs.t * Prep.t

           let pp f (job, _) = Fmt.pf f "%a" Jobs.pp job

           let compare (j1, _) (j2, _) = Jobs.compare j1 j2
         end )
         (fun prepped_job ->
           let prep = Current.map snd prepped_job in
           let blessed =
             let+ job, _ = prepped_job in
             job.Jobs.pkgs
           in
           compile prep blessed)
         prepped
  in
  assemble_and_link (Current.map (List.map snd) prepped) compiled
