let create parent_dir =
  let path = Fpath.(parent_dir / "opam-repository") in
  try
    Bos.OS.Dir.create ~path:true path |> ignore;
    Bos.OS.File.write Fpath.(path / "repo") {|opam-version: "2.0"|} |> ignore;
    Ok path
  with exn ->
    Rresult.R.error_msgf "Opam_repo.create %a: %s" Fpath.pp parent_dir
      (Printexc.to_string exn)

let build_merged ~dest opam_repositories =
  try
    if not (Bos.OS.Dir.exists dest |> Result.get_ok) then (
      Bos.OS.Dir.create ~path:true dest |> ignore;
      List.iteri
        (fun i repo ->
          let src = Fpath.v repo in
          if i = 0 then (
            let cmd =
              Printf.sprintf "cp -a %s/. %s/" (Fpath.to_string src)
                (Fpath.to_string dest)
            in
            if Sys.command cmd <> 0 then
              failwith (Printf.sprintf "cp -a failed for %s" repo))
          else
            let src_pkgs = Fpath.(src / "packages") in
            if Bos.OS.Dir.exists src_pkgs |> Result.get_ok then (
              let dst_pkgs = Fpath.(dest / "packages") in
              Bos.OS.Dir.create ~path:true dst_pkgs |> ignore;
              let cmd =
                Printf.sprintf "cp -a %s/. %s/" (Fpath.to_string src_pkgs)
                  (Fpath.to_string dst_pkgs)
              in
              if Sys.command cmd <> 0 then
                failwith (Printf.sprintf "cp -a failed for %s packages" repo)))
        opam_repositories);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Opam_repo.build_merged: %s" (Printexc.to_string exn)

let populate ~opam_repo ~opam_repositories packages =
  try
    List.iter
      (fun dep_pkg ->
        let name = OpamPackage.name_to_string dep_pkg in
        let pkg_str = OpamPackage.to_string dep_pkg in
        let rel = Fpath.(v "packages" / name / pkg_str) in
        List.find_map
          (fun repo ->
            let src = Fpath.(repo // rel) in
            if Bos.OS.Dir.exists src |> Result.get_ok then Some src else None)
          opam_repositories
        |> Option.iter (fun src ->
               let dst = Fpath.(opam_repo // rel) in
               Bos.OS.Dir.create ~path:true dst |> ignore;
               let src_opam = Fpath.(src / "opam") in
               if Bos.OS.File.exists src_opam |> Result.get_ok then
                 Bos.OS.File.read src_opam
                 |> Result.get_ok
                 |> Bos.OS.File.write Fpath.(dst / "opam")
                 |> ignore;
               let src_files = Fpath.(src / "files") in
               if Bos.OS.Dir.exists src_files |> Result.get_ok then (
                 let dst_files = Fpath.(dst / "files") in
                 Bos.OS.Dir.create dst_files |> ignore;
                 Sys.readdir (Fpath.to_string src_files)
                 |> Array.iter (fun f ->
                        let content =
                          Bos.OS.File.read Fpath.(src_files / f)
                          |> Result.get_ok
                        in
                        Bos.OS.File.write Fpath.(dst_files / f) content
                        |> ignore))))
      packages;
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Opam_repo.populate: %s" (Printexc.to_string exn)

(* Snapshot one package's slice into the layer dir, optionally
   inlining patches via opam's native [patches:] field. *)
let snapshot_to_layer ~layer_dir ~opam_repositories ?(patches = []) pkg =
  try
    let name = OpamPackage.name_to_string pkg in
    let pkg_str = OpamPackage.to_string pkg in
    let rel = Fpath.(v "packages" / name / pkg_str) in
    let src_opt =
      List.find_map
        (fun repo ->
          let src = Fpath.(repo // rel) in
          if Bos.OS.Dir.exists src |> Result.get_ok then Some src else None)
        opam_repositories
    in
    match src_opt with
    | None ->
        Rresult.R.error_msgf
          "Opam_repo.snapshot_to_layer: package %s not found in any of %d \
           source repos"
          pkg_str
          (List.length opam_repositories)
    | Some src ->
        let repo_dir = Fpath.(layer_dir / "opam-repository") in
        Bos.OS.Dir.create ~path:true repo_dir |> ignore;
        Bos.OS.File.write Fpath.(repo_dir / "repo") {|opam-version: "2.0"|}
        |> ignore;
        let dst = Fpath.(repo_dir // rel) in
        Bos.OS.Dir.create ~path:true dst |> ignore;
        let src_opam = Fpath.(src / "opam") in
        let dst_opam = Fpath.(dst / "opam") in
        let opam_content =
          if Bos.OS.File.exists src_opam |> Result.get_ok then
            Bos.OS.File.read src_opam |> Result.get_ok
          else ""
        in
        let dst_files = Fpath.(dst / "files") in
        let src_files = Fpath.(src / "files") in
        if Bos.OS.Dir.exists src_files |> Result.get_ok then (
          Bos.OS.Dir.create dst_files |> ignore;
          Sys.readdir (Fpath.to_string src_files)
          |> Array.iter (fun f ->
                 let content =
                   Bos.OS.File.read Fpath.(src_files / f) |> Result.get_ok
                 in
                 Bos.OS.File.write Fpath.(dst_files / f) content |> ignore));
        let patch_basenames =
          if patches = [] then []
          else (
            Bos.OS.Dir.create ~path:true dst_files |> ignore;
            List.mapi
              (fun i path ->
                let bn = Printf.sprintf "%03d-%s" i (Fpath.basename path) in
                let content = Bos.OS.File.read path |> Result.get_ok in
                Bos.OS.File.write Fpath.(dst_files / bn) content |> ignore;
                bn)
              patches)
        in
        (* Write the (possibly patched) opam file. If we added patches,
         parse the original, splice them into the [patches:] field, and
         re-serialise — opam will then apply them naturally at install
         time without needing the day11 [--patch] mechanism. *)
        let final_opam =
          if patch_basenames = [] then opam_content
          else
            let opam = OpamFile.OPAM.read_from_string opam_content in
            let existing = OpamFile.OPAM.patches opam in
            let new_patches =
              List.map
                (fun b -> (OpamFilename.Base.of_string b, None))
                patch_basenames
            in
            let opam' =
              OpamFile.OPAM.with_patches (existing @ new_patches) opam
            in
            OpamFile.OPAM.write_to_string opam'
        in
        Bos.OS.File.write dst_opam final_opam |> ignore;
        Ok ()
  with exn ->
    Rresult.R.error_msgf "Opam_repo.snapshot_to_layer: %s"
      (Printexc.to_string exn)
