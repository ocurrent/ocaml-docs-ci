(** rerun command: retry a failed build *)

open Cmdliner

let run profile_name profile_dir layer_hash =
  match Common.load_profile ~profile_dir ~name:profile_name with
  | Error (`Msg e) ->
      Printf.eprintf "Error: %s\n%!" e;
      1
  | Ok (_profile, paths) -> (
      Common.with_eio @@ fun ~sw env ->
      let os_dir = paths.os_dir in
      match Day11_opam_layer.Build_meta.load_tree env ~os_dir layer_hash with
      | Error (`Msg e) ->
          Printf.eprintf "Cannot load layer %s: %s\n" layer_hash e;
          1
      | Ok node -> (
          let cache_dir = paths.cache_dir in
          Printf.printf "Rerunning %s (%s)...\n%!"
            (OpamPackage.to_string node.pkg)
            (Day11_opam_layer.Build.dir_name node);
          match Day11_batch.Rerun.rerun ~sw env ~os_dir ~cache_dir node with
          | Day11_opam_build.Types.Success _ ->
              Printf.printf "Success\n";
              0
          | Day11_opam_build.Types.Failure msg ->
              Printf.printf "Failed: %s\n" msg;
              1
          | _ ->
              Printf.printf "Unexpected result\n";
              1))

let hash_term =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"HASH" ~doc:"Full layer hash to rerun")

let cmd =
  let info = Cmd.info "rerun" ~doc:"Retry a failed build" in
  let term =
    Term.(const run $ Common.profile_term $ Common.profile_dir_term $ hash_term)
  in
  Cmd.v info term
