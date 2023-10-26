let ( / ) = Filename.concat

module SS = Set.Make (String)

let remove ~root files =
  let num = SS.cardinal files in
  let () = Printf.printf "Deleting %i files\n" num in
  let _ = SS.fold (fun del (i, l) ->
    let pcent = Int.div (100 * i) num in
    let nl = if pcent > l then
        let () = Printf.printf "%i%%\r" pcent in
        let () = flush stdout in pcent
      else l in
    let _ = Sys.command ("rm -rf " ^ (root / del)) in
    (i + 1, nl)) files (0, 0) in
  Printf.printf "Completed.\n"

let print files =
  let total = SS.fold (fun del i ->
    let () = if i < 10 then Printf.printf "%s\n" del in
    i + 1) files 0 in
  if total >= 10 then
    Printf.printf "... plus %i more\n" (total - 10)

let main base_dir dry_run =
  let path = base_dir in

  let epochs =
    Sys.readdir path
    |> Array.to_list
    |> List.filter (fun file -> String.starts_with ~prefix:"epoch-" file)
    |> List.fold_left
         (fun acc epoch ->
           let full_path = path / epoch in
           List.map (fun sf -> full_path / sf) [ "html-raw/u"; "linked/u" ]
           @ acc)
         []
    |> List.filter Sys.file_exists in

  let epoch_files = List.fold_left (fun s epoch -> Array.fold_right SS.add (Sys.readdir epoch) s) SS.empty epochs in

  List.iter
    (fun universe ->
      let universes = path / universe in
      let univ_files = Array.fold_right SS.add (Sys.readdir universes) SS.empty in
      let debris = SS.diff univ_files epoch_files in
      let () = Printf.printf "Files to be deleted in %s:\n" universe in
      let () = print debris in
      if not dry_run then remove ~root:universes debris)
    [ "prep/universes"; "compile/u" ]

(* Command-line parsing *)

open Cmdliner

let base_dir =
  Arg.(required
       @@ opt (some dir) None
       @@ info ~docv:"BASE_DIR"
            ~doc: "Base directory containing epochs. eg /var/lib/docker/volumes/infra_docs-data/_data" ["base-dir"])

let dry_run =
  Arg.(value
    @@ flag
    @@ info ~docv:"DRY_RUN"
         ~doc: "If set, only list the files to be deleted but do not deleted them" ["dry-run"])

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "Epoch pruning" in
  let info = Cmd.info "epoch" ~doc ~version in
  Cmd.v info
    Term.(const main $ base_dir $ dry_run)

let () = exit @@ Cmd.eval cmd