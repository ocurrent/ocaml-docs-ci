(** debug command: interactive container for failed builds *)

open Cmdliner

let run profile_name profile_dir target keep command =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) -> (
      Common.with_eio @@ fun ~sw env ->
      let os_dir = paths.os_dir in
      (* Resolve target: try as a hash first, then as a package name via
     history lookup. [Build_meta.load_tree] reads [build.json] (and
     falls back to [layer.json] for legacy layers); both are written
     pre-attempt now, so failed builds load just fine. *)
      let node =
        match Day11_opam_layer.Build_meta.load_tree env ~os_dir target with
        | Ok n -> n
        | Error _ -> (
            let packages_dir =
              match Common.latest_snapshot_dir paths with
              | Some sd -> Fpath.(sd / "packages")
              | None -> Fpath.(os_dir / "packages")
            in
            let entries =
              Day11_lib.History.read ~packages_dir ~pkg_str:target
            in
            let failure =
              List.find_opt
                (fun (e : Day11_lib.History.entry) ->
                  e.status = "failure" && e.build_hash <> "none")
                entries
            in
            match failure with
            | Some e -> (
                Printf.printf "Using %s\n%!" e.build_hash;
                match
                  Day11_opam_layer.Build_meta.load_tree env ~os_dir e.build_hash
                with
                | Ok n -> n
                | Error (`Msg msg) ->
                    Printf.eprintf "Cannot load %s: %s\n" target msg;
                    exit 1)
            | None ->
                Printf.eprintf "No failed build found for %s\n" target;
                exit 1)
      in
      match Day11_opam_build.Debug.setup ~sw env ~os_dir ~keep node with
      | Error (`Msg e) ->
          Printf.eprintf "Setup failed: %s\n" e;
          1
      | Ok session ->
          Printf.printf "Debug container for %s\n%!"
            (OpamPackage.to_string session.pkg);
          let result =
            match command with
            | Some cmd -> Day11_opam_build.Debug.run_command ~sw env session cmd
            | None -> Day11_opam_build.Debug.run_interactive ~sw env session
          in
          if not keep then Day11_opam_build.Debug.teardown ~sw env session;
          if keep then
            Printf.printf "Debug dir kept at: %s\n%!"
              (Fpath.to_string session.temp_dir);
          result)

let target_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"TARGET" ~doc:"Package name or layer hash to debug")

let keep_term =
  let doc = "Keep debug container for re-entry" in
  Arg.(value & flag & info [ "keep" ] ~doc)

let command_term =
  let doc = "Run command instead of interactive shell" in
  Arg.(
    value & opt (some string) None & info [ "command"; "c" ] ~docv:"CMD" ~doc)

let cmd =
  let info = Cmd.info "debug" ~doc:"Launch interactive debug container" in
  let term =
    Term.(
      const run
      $ Common.profile_term
      $ Common.profile_dir_term
      $ target_term
      $ keep_term
      $ command_term)
  in
  Cmd.v info term
