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
    let solved =
      Current.collapse ~key:"solve" ~value:"" ~input:tracked
        (Current.list_map (module OpamPackage) (solve ~opam) tracked)
    in
    Current.collapse ~key:"explode" ~value:"" ~input:solved
      (Current.list_map
         (module Package)
         (fun pkg ->
           let universe =
             let+ pkg = pkg in
             Package.universe pkg
           in
           let+ ex = explode ~opam universe and+ pkg = pkg in
           { Jobs.root = pkg; pkgs = pkg :: ex })
         solved)
  in
  let blessed_packages =
    all_packages_jobs
    |> Current.map (List.map (fun x -> x.Jobs.pkgs))
    |> Current.map List.flatten |> bless_packages
  in
  let jobs = select_jobs ~targets:all_packages_jobs ~blessed:blessed_packages in
  let prepped =
    Current.collapse ~key:"prep" ~value:"" ~input:jobs @@
    let+ res =
      Current.list_map
        (module Jobs)
        (fun job ->
          Current.pair job (build_and_prep (Current.map (fun x -> x.Jobs.root) job))
          |> Current.catch ~hidden:true)
        jobs
    in
    List.filter_map Result.to_option res
  in
  let linked =
    Current.collapse ~key:"link" ~value:"" ~input:prepped @@
    Current.list_map
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
        link prep blessed)
      prepped
  in
  Current.ignore_value linked
