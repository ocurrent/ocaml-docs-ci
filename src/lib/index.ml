let record (package : Package.t) (config : Config.t)
    (step_list : Monitor.step list) (voodoo_do_commit : string)
    (voodoo_gen_commit : string) =
  Log.info (fun f ->
      f
        "[Index] Package: %s:%s Voodoo-branch: %s Voodoo-repo: %s \
         Voodoo-do-commit: %s Voodoo-gen-commit: %s Step-list: %a"
        (Package.opam package |> OpamPackage.name_to_string)
        (Package.opam package |> OpamPackage.version_to_string)
        (Config.voodoo_branch config)
        (Config.voodoo_repo config)
        voodoo_do_commit voodoo_gen_commit
        (Format.pp_print_list Monitor.pp_step)
        step_list);
  ()
